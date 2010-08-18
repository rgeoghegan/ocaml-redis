(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Main library file. *)

type redis_value_type = RedisString | RedisNil | RedisList | RedisSet | RedisZSet
type bulk_data = Nil | String of string
type rank = NilRank | Rank of int
type multi_bulk_data = MultibulkNil | MultibulkValue of bulk_data list
exception RedisServerError of string
exception RedisNilError of string
exception RedisInvalidArgumentError of string

(* This is an internal type used to transform what the Redis server can handle into ocaml types. *)
type response = Status of string | Undecipherable | Integer of int | LargeInteger of float | Bulk of bulk_data | Multibulk of multi_bulk_data | Error of string

(* Different printing functions for the above types *)

let string_of_bulk_data bd =
    match bd with
        String(x) -> x |
        Nil -> raise (RedisNilError "Trying to extract string from none")

let int_of_rank r =
    match r with
        Rank(x) -> x |
        NilRank -> raise (RedisNilError "Trying to extract string from none")

let string_of_response r =
    let bulk_printer x =
        match x with
            Nil -> "Nil"
            | String(d) -> Printf.sprintf "String(%S)" d
    in
    let rec multi_bulk_list_to_string l =
        match l with
            [] -> ""
            | h :: t -> Printf.sprintf "; %s%s" (bulk_printer h) (multi_bulk_list_to_string t)
    in
    match r with
        Status(x) -> Printf.sprintf "Status(%S)" x |
        Undecipherable -> "Undecipherable" |
        Integer(x) -> Printf.sprintf "Integer(%d)" x |
        LargeInteger(x) -> Printf.sprintf "LargeInteger(%.2f)" x |
        Error(x) -> Printf.sprintf "Error(%S)" x |
        Bulk(x) -> Printf.sprintf "Bulk(%s)" (bulk_printer x) |
        Multibulk(x) -> match x with
            MultibulkNil -> "MultibulkNil" |
            MultibulkValue([]) -> Printf.sprintf "Multibulk([])" |
            MultibulkValue(h :: t) -> Printf.sprintf "Multibulk([%s%s])" (bulk_printer h) (multi_bulk_list_to_string t)

let string_of_redis_value_type vt =
    match vt with
        RedisNil -> "Nil" |
        RedisString -> "String" |
        RedisList -> "List" |
        RedisSet -> "Set" |
        RedisZSet -> "ZSet"

let debug_string comment value  = begin
    (* Simple function to print out a msg to stdout, should not be used in production code. *)
    Printf.printf "%s %S\n" comment value;
    flush stdout
end


(* The Connection module handles some low level operations with the sockets *)
module Connection =
    struct
        type t = in_channel * out_channel
        let create addr port =
            let server = Unix.inet_addr_of_string addr
            in
            let in_c, out_c = Unix.open_connection(
                Unix.ADDR_INET(server, port)
            )
            in
                (in_c, out_c)

        let read_string (in_chan, _) =
            (* Read arbitratry length string (hopefully quite short) from current pos in in_chan until \r\n *)
            let rec iter out_buffer =
                match input_char in_chan with
                    '\r' -> (
                        match input_char in_chan with
                            '\n' -> Buffer.contents out_buffer |
                            x -> begin
                                Buffer.add_char out_buffer '\r';
                                Buffer.add_char out_buffer x;
                                iter out_buffer
                            end 
                    ) |
                    x -> begin
                        Buffer.add_char out_buffer x;
                        iter out_buffer
                    end
            in
            iter (Buffer.create 100)

        let read_fixed_string length (in_chan, _) =
            (* Read length caracters from in channel and output them as string *)
            let out_buf = Buffer.create length
            in begin
                Buffer.add_channel out_buf in_chan length;
                Buffer.contents out_buf
            end;;

        let send_text_straight text (_, out_chan) =
            (* Send the given text out to the connection without flushing *)
            begin
                output_string out_chan text;
                output_string out_chan "\r\n"
            end

        let send_text text connection =
            (* Send the given text out to the connection *)
            let (_, out_chan) = connection
            in
            begin
                send_text_straight text connection;
                flush out_chan
            end
        
        let get_one_char (in_chan, out_chan) =
            (* Retrieve one character from the connection *)
            input_char in_chan

        let flush_connection (in_chan, out_chan) =
            (* Manually flushes out the outgoing channel *)
            flush out_chan
    end;;

module Redis_util =
    struct
        let get_bulk_data connection =
            (* Gets the data from a '$x\r\nx\r\n' response, once the first '$' has already been
            popped off *)
            let length = int_of_string (Connection.read_string connection)
            in
            if length == -1
            then Nil
            else let out_str = if length == 0
                    then String("")
                    else String(Connection.read_fixed_string length connection)
                in
                begin
                    (* Exhausts the \r\n ending of a bulk data value *)
                    List.iter (fun x -> assert(x == Connection.get_one_char connection)) ['\r'; '\n'];
                    out_str
                end

        let get_multibulk_data size conn =
            (* Parse multibulk data structure, with first '*' already popped off *)
            let in_chan, _ = conn
            in
            let rec iter i data =
                if i == 0
                then Multibulk(MultibulkValue(List.rev data))
                else match input_char in_chan with
                    '$' -> iter
                        (i - 1)
                        ((get_bulk_data conn) :: data)
                    | _ -> Undecipherable
            in
            match size with
                -1 -> Multibulk(MultibulkNil) |
                0 -> Multibulk(MultibulkValue([])) |
                x when x > 0 -> iter size [] |
                _ -> Undecipherable

        let parse_integer_response response =
            (* Expecting an integer response, will return the right type depending on the size *)
            try
                Integer(int_of_string response)
            with Failure "int_of_string" ->
                LargeInteger(float_of_string response)

        let handle_error reply =
            (* Filters out any errors and raises them.
                Raises RedisServerError with the error message if getting an explicit error from the server ("-...")
                or a RedisServerError with "Could not decipher response" for an unrecognised response type. *)
            match reply with
                Error(e) -> raise (RedisServerError e) |
                Undecipherable -> raise (RedisServerError "Could not decipher response") |
                x -> x;;

        let receive_answer connection =
            (* Get answer back from redis and cast it to the right type *)
            match (Connection.get_one_char connection) with
                '+' -> Status(Connection.read_string connection)
                | ':' -> parse_integer_response (Connection.read_string connection)
                | '$' -> Bulk(
                        get_bulk_data connection
                    )
                | '*' -> get_multibulk_data (int_of_string (Connection.read_string connection)) connection
                | '-' -> Error(Connection.read_string connection)
                | _ -> begin
                    ignore (Connection.read_string connection);
                    Undecipherable
                end

        let send_and_receive_command command connection =
            (* Send command, and receive the result casted to the right type *)
            begin
                Connection.send_text command connection;
                receive_answer connection
            end

        let send_and_receive_command_safely command connection =
            (* Will send the command, much like send_and_receive_command, but will catch and failwith any errors *)
            handle_error (send_and_receive_command command connection)

        let send_with_value_and_receive_command_safely command value connection =
            (* Will send out the command, appended with the length of value, and will then send out value. Also
            will catch and fail on any errors. I.e., given 'foo' 'bar', will send "foo 3\r\nbar\r\n" *)
            begin
                Connection.send_text_straight (command ^ " " ^ (string_of_int (String.length value))) connection;
                Connection.send_text value connection;
                handle_error (receive_answer connection) 
            end

        let aggregate_command command tokens = 
            (* Given a list of tokens, joins them with command *)
            let joiner buf new_item = begin
                    Buffer.add_char buf ' ';
                    Buffer.add_string buf new_item
                end
            in
            match tokens with 
                [] -> failwith "Need at least one key"
                | x ->
                    let out_buffer = Buffer.create 256
                    in begin
                            Buffer.add_string out_buffer command;
                            List.iter (joiner out_buffer) x;
                            Buffer.contents out_buffer
                        end

        let send_multibulk_command tokens connection =
            (* Will send a given list of tokens to send in multibulk. *)
            let token_length = (string_of_int (List.length tokens))
            in
            let rec send_in_tokens tokens_left =
                match tokens_left with
                    [] -> () |
                    h :: t -> begin
                        Connection.send_text_straight ("$" ^ (string_of_int (String.length h))) connection;
                        Connection.send_text_straight h connection;
                        send_in_tokens t
                    end
            in
            begin
                Connection.send_text_straight ("*" ^ token_length) connection;
                send_in_tokens tokens;
                Connection.flush_connection connection;
                receive_answer connection
            end

        let handle_special_status special_status reply =
            (* For status replies, does error checking and display *)
            match handle_error reply with
                Status(x) when x = special_status -> () |
                Status(x) -> failwith ("Received status(" ^ x ^ ")") |
                _ -> failwith "Did not recognize what I got back"

        let handle_status = handle_special_status "OK"
            (* The most common case of handle_special_status is the "OK" status *)

        let handle_integer_as_boolean reply =
            (* For integer replies, does error checking and casting to boolean *)
            match handle_error reply with
                Integer(0) -> false |
                Integer(1) -> true |
                _ -> failwith "Did not recognize what I got back"

        let handle_float reply =
            (* For bulk replies that should be floating point numbers, does error checking and casts to float *)
            match handle_error reply with
                Bulk(String(x)) ->
                    (try
                        float_of_string x 
                    with Failure "float_of_string" ->
                        failwith (Printf.sprintf "%S is not a floating point number" x) ) |
                Bulk(Nil) | _ -> failwith "Did not recognize what I got back"

        let expect_non_nil_multibulk reply =
            (* Extracts bulk_data_list out of Multibulk replies, raises and error if a nil is found *)
            match reply with
                Multibulk(MultibulkNil) -> raise (RedisNilError "Was not expecting MultibulkNil response.") |
                Multibulk(MultibulkValue(v)) -> v |
                _ -> failwith "Did not recognize what I got back"
    end;;

open Redis_util;;

let create_connection ?addr:(addr="127.0.0.1") ?port:(port=6379) () =
    (* From a string of the address, and a port as an int, gets an input and output file discriptor *)
    Connection.create addr port;;

(* Individual commands *)

(* Connection handling *)
let ping connection =
    (* PING *)
    match send_and_receive_command_safely "PING" connection with
        Status("PONG") -> true |
        _ -> failwith "Did not recognize what I got back";;

let quit connection =
    (* QUIT, also should automatically close the connection *)
    Connection.send_text "QUIT" connection;;

let auth password connection =
    (* AUTH *)
    handle_status (send_and_receive_command ("AUTH " ^ password) connection);;

(* Commands operating on string values *)
let set key value connection =
    (* SET *)
    handle_status
        (send_with_value_and_receive_command_safely ("SET " ^ key) value connection);;

let get key connection =
    (* GET *)
    match send_and_receive_command_safely ("GET " ^ key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let getset key new_value connection =
    (* GETSET *)
    match send_with_value_and_receive_command_safely ("GETSET " ^ key) new_value connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let mget keys connection = 
    (* MGET *)
    expect_non_nil_multibulk 
        (send_and_receive_command_safely (aggregate_command "MGET" keys) connection);;

let setnx key value connection =
    (* SETNX *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("SETNX " ^ key) value connection);;

let setex key timeout value connection =
    (* SETEX *)
    handle_status
        (send_with_value_and_receive_command_safely
            (Printf.sprintf "SETEX %s %d" key timeout) value connection);;

let mset key_value_pairs connection =
    (* MSET *)
    let rec flatten list_of_pairs result =
        match list_of_pairs with
            (key, value) :: tail -> flatten tail (key :: value :: result) |
            [] -> result
    in
    handle_status (send_multibulk_command ( "MSET" :: (flatten key_value_pairs [])) connection)

let msetnx key_value_pairs connection =
    (* MSETNX *)
    let rec flatten list_of_pairs result =
        match list_of_pairs with
            (key, value) :: tail -> flatten tail (key :: value :: result) |
            [] -> result
    in
    handle_integer_as_boolean (send_multibulk_command ( "MSETNX" :: (flatten key_value_pairs [])) connection)

let incr key connection =
    (* INCR *)
    match send_and_receive_command_safely (Printf.sprintf "INCR %s" key) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let incrby key value connection =
    (* INCRBY *)
    match send_and_receive_command_safely (Printf.sprintf "INCRBY %s %d" key value) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let decr key connection =
    (* DECR *)
    match send_and_receive_command_safely (Printf.sprintf "DECR %s" key) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let decrby key value connection =
    (* DECRBY *)
    match send_and_receive_command_safely (Printf.sprintf "DECRBY %s %d" key value) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let append key value connection =
    (* APPEND *)
    match send_with_value_and_receive_command_safely ("APPEND " ^ key) value connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let substr key start stop connection =
    (* SUBSTR, note that the word 'end' is a keyword in ocaml, so it has been replaced by 'stop' *)
    match send_and_receive_command_safely (Printf.sprintf "SUBSTR %s %d %d" key start stop) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let exists key connection =
    (* EXISTS *)
    handle_integer_as_boolean (send_and_receive_command ("EXISTS " ^ key) connection)

let del keys connection =
    (* DEL *)
    match send_and_receive_command_safely (aggregate_command "DEL" keys) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let del_one key connection =
    (* Exactly like "del", except you do not need to provide a list, just one key. Not in spec *)
    handle_integer_as_boolean (send_and_receive_command ("DEL " ^ key) connection);;

let value_type key connection =
    (* TYPE, unfortunately type is an ocaml keyword, so it cannot be used as a function name *)
    match send_and_receive_command_safely ("TYPE " ^ key) connection with
        Status("string") -> RedisString |
        Status("set") -> RedisSet |
        Status("zset") -> RedisZSet |
        Status("list") -> RedisList |
        Status("none") -> RedisNil |
        _ -> failwith "Did not recognize what I got back";;
    
(* Commands operating on the key space *)

let keys pattern connection =
    (* KEYS *)
    match send_and_receive_command_safely ("KEYS " ^ pattern) connection with
        Multibulk(MultibulkValue(l)) -> List.map string_of_bulk_data l |
        _ -> failwith "Did not recognize what I got back";;

let randomkey connection =
    (* RANDOMKEY *)
    match send_and_receive_command_safely "RANDOMKEY" connection with
        Bulk(x) -> string_of_bulk_data x |
        _ -> failwith "Did not recognize what I got back";;
        
let rename oldkey newkey connection =
    (* RENAME *)
    handle_status (send_and_receive_command (Printf.sprintf "RENAME %s %s" oldkey newkey) connection);;

let renamenx oldkey newkey connection =
    (* RENAMENX *)
    handle_integer_as_boolean (send_and_receive_command (Printf.sprintf "RENAMENX %s %s" oldkey newkey) connection);;

let dbsize connection =
    (* DBSIZE *)
    match send_and_receive_command_safely  "DBSIZE" connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let expire key seconds connection =
    (* EXPIRE *)
    handle_integer_as_boolean (send_and_receive_command (Printf.sprintf "EXPIRE %s %d" key seconds) connection);;

let expireat key time connection =
    (* EXPIREAT *)
    handle_integer_as_boolean (send_and_receive_command (Printf.sprintf "EXPIREAT %s %.f" key time) connection);;

let ttl key connection =
    (* TTL *)
    match send_and_receive_command_safely ("TTL " ^ key) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

(* Commands operating on lists *)
let rpush key value connection =
    (* RPUSH *)
    match send_with_value_and_receive_command_safely ("RPUSH " ^ key) value connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let lpush key value connection =
    (* LPUSH *)
    match send_with_value_and_receive_command_safely ("LPUSH " ^ key) value connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let llen key connection =
    (* LLEN *)
    match send_and_receive_command_safely ("LLEN " ^ key) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let lrange key start stop connection =
    (* LRANGE, please note that the word 'end' is a keyword in ocaml, so it has been replaced by 'stop' *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely (Printf.sprintf "LRANGE %s %d %d" key start stop) connection);;

let ltrim key start stop connection =
    (* LTRIM, please note that the word 'end' is a keyword in ocaml, so it has been replaced by 'stop' *)
    match send_and_receive_command_safely (Printf.sprintf "LTRIM %s %d %d" key start stop) connection with
        Status("OK") -> () |
        _ -> failwith "Did not recognize what I got back";;

let lindex key index connection =
    (* LINDEX *)
    match send_and_receive_command_safely (Printf.sprintf "LINDEX %s %d" key index) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let lset key index value connection =
    (* LSET *)
    handle_status
        (send_with_value_and_receive_command_safely (Printf.sprintf "LSET %s %d" key index) value connection);;

let lrem key count value connection =
    (* LREM *)
    match send_with_value_and_receive_command_safely (Printf.sprintf "LREM %s %d" key count) value connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let lpop key connection =
    (* LPOP *)
    match send_and_receive_command_safely ("LPOP " ^ key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let rpop key connection =
    (* RPOP *)
    match send_and_receive_command_safely ("RPOP " ^ key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let rpoplpush src_key dest_key connection =
    (* RPOPLPUSH *)
    match send_and_receive_command_safely ("RPOPLPUSH " ^ src_key ^ " " ^ dest_key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let blpop key ?(timeout=`None) connection =
    (* BLPOP, but for only one key *)
    match send_and_receive_command_safely
            (Printf.sprintf
                "BLPOP %s %d"
                key
                (match timeout with
                    `None -> 0 |
                    `Seconds(s) -> s)) connection
        with
            Multibulk(MultibulkValue([key; v])) -> v |
            Multibulk(MultibulkNil) -> Nil |
            _ -> failwith "Did not recognize what I got back";;

let blpop_many key_list ?(timeout=`None) connection =
    (* BLPOP *)
    match send_and_receive_command_safely
            (Printf.sprintf
                "BLPOP %s %d"
                (List.fold_left
                    (fun rest n -> rest ^ " " ^ n)
                    (List.hd key_list)
                    (List.tl key_list))
                (match timeout with
                    `None -> 0 |
                    `Seconds(s) -> s)) connection
        with
            Multibulk(MultibulkValue([key; v])) -> ((string_of_bulk_data key), v) |
            Multibulk(MultibulkNil) -> ("", Nil) |
            _ -> failwith "Did not recognize what I got back";;

let brpop key ?(timeout=`None) connection =
    (* BRPOP, but for only one key *)
    match send_and_receive_command_safely
            (Printf.sprintf
                "BRPOP %s %d"
                key
                (match timeout with
                    `None -> 0 |
                    `Seconds(s) -> s)) connection
        with
            Multibulk(MultibulkValue([key; v])) -> v |
            Multibulk(MultibulkNil) -> Nil |
            _ -> failwith "Did not recognize what I got back";;

let brpop_many key_list ?(timeout=`None) connection =
    (* BRPOP *)
    match send_and_receive_command_safely
            (Printf.sprintf
                "BRPOP %s %d"
                (List.fold_left
                    (fun rest n -> rest ^ " " ^ n)
                    (List.hd key_list)
                    (List.tl key_list))
                (match timeout with
                    `None -> 0 |
                    `Seconds(s) -> s)) connection
        with
            Multibulk(MultibulkValue([key; v])) -> ((string_of_bulk_data key), v) |
            Multibulk(MultibulkNil) -> ("", Nil) |
            _ -> failwith "Did not recognize what I got back";;

(* Commands operating on sets *)
let sadd key member connection =
    (* SADD *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("SADD " ^ key) member connection);;

let srem key member connection =
    (* SREM *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("SREM " ^ key) member connection);;

let spop key connection =
    (* SPOP *)
    match send_and_receive_command_safely ("SPOP " ^ key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let smove srckey destkey member connection =
    (* SMOVE *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely
            (Printf.sprintf "SMOVE %s %s" srckey destkey)
            member
            connection);;

let scard key connection =
    (* SCARD *)
    match send_and_receive_command_safely ("SCARD " ^ key) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let sismember key member connection =
    (* SISMEMBER *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("SISMEMBER " ^ key) member connection);;

let smembers key connection =
    (* SMEMBERS *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely ("SMEMBERS " ^ key) connection);;

let sinter keys connection =
    (* SINTER *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely (aggregate_command "SINTER" keys) connection);;

let sinterstore dstkey keys connection =
    (* SINTERSTORE *)
    match send_and_receive_command_safely (aggregate_command "SINTERSTORE" (dstkey :: keys)) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let sunion keys connection =
    (* SUNION *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely (aggregate_command "SUNION" keys) connection);;

let sunionstore dstkey keys connection =
    (* SUNIONSTORE *)
    match send_and_receive_command_safely (aggregate_command "SUNIONSTORE" (dstkey :: keys)) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let sdiff from_key keys connection =
    (* SDIFF *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely (aggregate_command "SDIFF" (from_key :: keys)) connection);;

let sdiffstore dstkey from_key keys connection =
    (* SDIFFSTORE *)
    match send_and_receive_command_safely (aggregate_command "SDIFFSTORE" (dstkey :: from_key :: keys)) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let srandmember key connection =
    (* SRANDMEMBER *)
    match send_and_receive_command_safely ("SRANDMEMBER " ^ key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

(* Multiple databases handling commands *)
let select index connection =
    (* SELECT *)
    handle_status
        (send_and_receive_command
            ("SELECT " ^
                (string_of_int index))
            connection);;

let move key index connection =
    (* MOVE *)
    handle_integer_as_boolean (send_and_receive_command ( Printf.sprintf "MOVE %s %d" key index ) connection)

let flushdb connection =
    (* FLUSHDB *)
    handle_status (send_and_receive_command "FLUSHDB" connection);;

let flushall connection =
    (* FLUSHALL *)
    handle_status (send_and_receive_command "FLUSHALL" connection);;

(* Commands operating on sorted sets *)
let zadd key score member connection =
    (* ZADD *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely (Printf.sprintf "ZADD %s %f" key score) member connection);;

let zrem key member connection =
    (* ZREM *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("ZREM " ^ key) member connection);;

let zrange key start stop connection =
    (* ZRANGE, please note that the word 'end' is a keyword in ocaml, so it has been replaced by 'stop' *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely (Printf.sprintf "ZRANGE %s %d %d" key start stop) connection);;

let score_transformer value_and_scores_list =
    (* Takes a list of [v_1; s_1; v_2; s_2; ...; v_n; s_n] and
    collates it into a list of pairs [(v_1, s_1); (v_2, s_2); ... ; (v_n, s_n)] *)
    let rec value_iter l out =
        match l with
            [] -> List.rev out |
            h :: t -> score_iter t h out
    and score_iter l value out =
        match l with
            h :: t ->
                let score = float_of_string (string_of_bulk_data h)
                in
                value_iter t ((value, score) :: out) |
            [] -> failwith ("Missing score for value " ^ (string_of_bulk_data value))
    in
        value_iter value_and_scores_list [];;
    
let zrange_withscores key start stop connection =
    (* ZRANGE, but with the WITHSCORES option added on. *)
    score_transformer
        (expect_non_nil_multibulk
            (send_and_receive_command_safely
                (Printf.sprintf "ZRANGE %s %d %d WITHSCORES" key start stop)
                connection));;

let zrevrange key start stop connection =
    (* ZREVRANGE, please note that the word 'end' is a keyword in ocaml, so it has been replaced by 'stop' *)
    expect_non_nil_multibulk
        (send_and_receive_command_safely (Printf.sprintf "ZREVRANGE %s %d %d" key start stop) connection);;

let zrevrange_withscores key start stop connection =
    (* ZRANGE, but with the WITHSCORES option added on. *)
    score_transformer
        (expect_non_nil_multibulk
            (send_and_receive_command_safely
                (Printf.sprintf "ZREVRANGE %s %d %d WITHSCORES" key start stop)
                connection));;

let zrangebyscore key start stop ?(limit=`Unlimited) connection =
    (* ZRANGEBYSCORE, please note that the word 'end' is a keyword in ocaml, so it has been replaced by 'stop' *)
    let command =
        let limit = match limit with
            `Unlimited -> "" |
            `Limit(x,y) -> (Printf.sprintf " LIMIT %d %d" x y)
        in
        Printf.sprintf "ZRANGEBYSCORE %s %f %f%s" key start stop limit
    in
    expect_non_nil_multibulk
        (send_and_receive_command_safely command connection);;

let zincrby key increment member connection =
    (* ZINCRBY *)
    handle_float
        (send_with_value_and_receive_command_safely (Printf.sprintf "ZINCRBY %s %f" key increment) member connection);;

let zrank key member connection =
    (* ZRANK *)
    match send_with_value_and_receive_command_safely ("ZRANK " ^ key) member connection with
        Integer(x) -> Rank(x) |
        Bulk(Nil) -> NilRank |
        _ -> failwith "Did not recognize what I got back";;

let zrevrank key member connection =
    (* ZREVRANK *)
    match send_with_value_and_receive_command_safely ("ZREVRANK " ^ key) member connection with
        Integer(x) -> Rank(x) |
        Bulk(Nil) -> NilRank |
        _ -> failwith "Did not recognize what I got back";;
        
let zcard key connection =
    (* ZCARD *)
    match (send_and_receive_command_safely ("ZCARD " ^ key) connection) with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let zscore key member connection =
    (* ZSCORE *)
    handle_float
        (send_with_value_and_receive_command_safely ("ZSCORE " ^ key) member connection);;

let zremrangebyrank key start stop connection =
    (* ZREMRANGEBYRANK *)
    match send_and_receive_command_safely
        (Printf.sprintf "ZREMRANGEBYRANK %s %d %d" key start stop) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let zremrangebyscore key min max connection =
    (* ZREMRANGEBYSCORE *)
    match send_and_receive_command_safely (Printf.sprintf "ZREMRANGEBYSCORE %s %f %f" key min max) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let zunionstore dstkey key_list ?(aggregate=`Sum) connection =
    match send_and_receive_command_safely
        (Printf.sprintf "ZUNIONSTORE %s %d%s AGGREGATE %s" dstkey
            (List.length key_list)
            (List.fold_left (fun rest x -> rest ^ " " ^ x) "" key_list)
            (match aggregate with
                `Sum -> "SUM" | `Min -> "MIN" | `Max -> "MAX"))
        connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let zunionstore_withweights dstkey key_list weight_list ?(aggregate=`Sum) connection =
    if List.length key_list != List.length weight_list
    then raise (RedisInvalidArgumentError("Not as many weights were given as keys to zunionstore_withweights"))
    else match send_and_receive_command_safely
        (Printf.sprintf "ZUNIONSTORE %s %d%s WEIGHTS%s AGGREGATE %s" dstkey
            (List.length key_list)
            (List.fold_left (fun rest x -> rest ^ " " ^ x) "" key_list)
            (List.fold_left (fun rest x -> Printf.sprintf "%s %f" rest x) "" weight_list)
            (match aggregate with
                `Sum -> "SUM" | `Min -> "MIN" | `Max -> "MAX"))
        connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let zinterstore dstkey key_list ?(aggregate=`Sum) connection =
    match send_and_receive_command_safely
        (Printf.sprintf "ZINTERSTORE %s %d%s AGGREGATE %s" dstkey
            (List.length key_list)
            (List.fold_left (fun rest x -> rest ^ " " ^ x) "" key_list)
            (match aggregate with
                `Sum -> "SUM" | `Min -> "MIN" | `Max -> "MAX"))
        connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let zinterstore_withweights dstkey key_list weight_list ?(aggregate=`Sum) connection =
    if List.length key_list != List.length weight_list
    then raise (RedisInvalidArgumentError("Not as many weights were given as keys to zinterstore_withweights"))
    else match send_and_receive_command_safely
        (Printf.sprintf "ZINTERSTORE %s %d%s WEIGHTS%s AGGREGATE %s" dstkey
            (List.length key_list)
            (List.fold_left (fun rest x -> rest ^ " " ^ x) "" key_list)
            (List.fold_left (fun rest x -> Printf.sprintf "%s %f" rest x) "" weight_list)
            (match aggregate with
                `Sum -> "SUM" | `Min -> "MIN" | `Max -> "MAX"))
        connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

(* Commands operating on hashes *)

let hset key field value connection =
    (* HSET *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely (Printf.sprintf "HSET %s %s" key field) value connection);;

let hdel key field connection =
    (* HDEL *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("HDEL " ^ key) field connection);;

let hget key field connection =
    (* HGET *)
    match send_with_value_and_receive_command_safely ("HGET " ^ key) field connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let hmget key field_list connection =
    (* HMGET *)
    let cmd_buf = Buffer.create 256
    in
    (* Unfortunately, HMGET has the list of fields inline, except for the last field, which is given as a value *)
    let rec stick_together_but_leave_last cmds =
        match cmds with
            x :: [] -> x |
            x :: t -> begin
                Buffer.add_char cmd_buf ' ';
                Buffer.add_string cmd_buf x;
                stick_together_but_leave_last t
            end |
            [] -> failwith "Something went wrong, not expecting an empty list."
    in
    begin
        Buffer.add_string cmd_buf "HMGET";
        let value = stick_together_but_leave_last (key :: field_list)
        in
            match send_with_value_and_receive_command_safely (Buffer.contents cmd_buf) value connection with
                Multibulk(MultibulkValue(x)) -> x |
                _ -> failwith "Did not recognize what I got back"
    end;;

let hmset key field_value_pairs connection =
    (* HMSET *)
    let rec flatten list_of_pairs result =
        match list_of_pairs with
            (key, value) :: tail -> flatten tail (key :: value :: result) |
            [] -> result
    in
    handle_status
        (send_multibulk_command
           ( "HMSET" :: key :: (flatten (List.rev field_value_pairs) [])) connection);;

let hincrby key field value connection =
    (* HINCRBY *)
    match send_and_receive_command_safely (Printf.sprintf "HINCRBY %s %s %d" key field value) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let hexists key field connection =
    (* HEXISTS *)
    handle_integer_as_boolean
        (send_with_value_and_receive_command_safely ("HEXISTS " ^ key) field connection);;

let hlen key connection =
    (* HLEN *)
    match send_and_receive_command_safely ("HLEN " ^ key) connection with
        Integer(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let hkeys key connection =
    (* HKEYS *)
    match send_and_receive_command_safely ("HKEYS " ^ key) connection with
        Multibulk(MultibulkValue(x)) -> List.map string_of_bulk_data x |
        _ -> failwith "Did not recognize what I got back";;

let hvals key connection =
    (* HVALS *)
    match send_and_receive_command_safely ("HVALS " ^ key) connection with
        Multibulk(MultibulkValue(x)) -> List.map string_of_bulk_data x |
        _ -> failwith "Did not recognize what I got back";;

let hgetall key connection =
    (* HGETALL *)
    let rec collate_pairs elems out =
        match elems with
            [] -> List.rev out |
            f :: v :: rest -> collate_pairs rest ((f,v) :: out) |
            _ -> failwith "Did not provide a pair of field-values"
    in
    match send_and_receive_command_safely ("HGETALL " ^ key) connection with
        Multibulk(MultibulkValue(x)) -> collate_pairs (List.map string_of_bulk_data x) [] |
        _ -> failwith "Did not recognize what I got back";;

(* Sorting *)

type redis_sort_pattern = KeyPattern of string | FieldPattern of string * string | NoSort | NoPattern

let parse_sort_args pattern limit order alpha =
    (* Some of the sort args need further parsing and are used across multiple functions. *)
    let pattern = match pattern with
        KeyPattern(k) -> " BY " ^ k |
        FieldPattern(k, f) -> Printf.sprintf " BY %s->%s" k f |
        NoSort -> " BY nosort" |
        NoPattern -> ""
    in
    let limit = match limit with
        `Unlimited -> "" |
        `Limit(x,y) -> (Printf.sprintf " LIMIT %d %d" x y)
    in
    let order = match order with
        `Asc -> "" |
        `Desc -> " DESC"
    in
    let alpha = match alpha with
        `NonAlpha -> "" |
        `Alpha -> " ALPHA"
    in
        (pattern, limit, order, alpha);;

let parse_get_arg get =
    (* The get argument needs specific parsing *)
    match get with
        KeyPattern(k) -> " GET " ^ k |
        FieldPattern(k, f) -> Printf.sprintf " GET %s->%s" k f |
        NoSort | NoPattern -> "";;
    
let sort key
    ?(pattern=NoPattern)
    ?(limit=`Unlimited)
    ?(get=NoPattern)
    ?(order=`Asc)
    ?(alpha=`NonAlpha)
        connection =
    (* SORT *)
    let command = 
        let pattern, limit, order, alpha =
            parse_sort_args pattern limit order alpha
        in
        "SORT " ^ key ^ pattern ^ limit ^ (parse_get_arg get) ^ order ^ alpha
    in
        expect_non_nil_multibulk (send_and_receive_command_safely command connection);;

let sort_get_many key get_patterns
    ?(pattern=NoPattern)
    ?(limit=`Unlimited)
    ?(order=`Asc)
    ?(alpha=`NonAlpha)
        connection =
    (* SORT, for multiple gets *)
    let command = 
        let pattern, limit, order, alpha =
            parse_sort_args pattern limit order alpha
        in
        let get = List.fold_left
            (fun rest n -> rest ^ " GET " ^ n)
            ""
            get_patterns
        in
        "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha
    in
    let collate_response count responses =
        (* Collates the returned list into a series of lists matching the 'GET' parameter *)
        let rec iter x whats_left current_response all_responses =
            match (x, whats_left) with
                (0, _) -> iter count whats_left [] ((List.rev current_response) :: all_responses) |
                (count, []) -> List.rev all_responses |
                (_, h::t) -> iter (x - 1) t (h :: current_response) all_responses
        in
            iter count responses [] []
    in
        collate_response
            (List.length get_patterns)
            (expect_non_nil_multibulk
                (send_and_receive_command_safely command connection));;

let sort_and_store key get_patterns dest_key
    ?(pattern=NoPattern)
    ?(limit=`Unlimited)
    ?(order=`Asc)
    ?(alpha=`NonAlpha)
        connection =
    (* SORT, with the STORE keyword *)
    let command = 
        let pattern, limit, order, alpha =
            parse_sort_args pattern limit order alpha
        in
        let get = List.fold_left
            (fun rest n -> rest ^ " GET " ^ n)
            ""
            get_patterns
        in
        let store = " STORE " ^ dest_key
        in
        "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha ^ store
    in
        match send_and_receive_command_safely command connection with
            Integer(x) -> x |
            _ -> failwith "Did not recognize what I got back";;

(* Persistence control commands *)
let save connection =
    (* SAVE *)
    handle_status (send_and_receive_command "SAVE" connection);;

let bgsave connection =
    (* BGSAVE *)
    handle_special_status "Background saving started" (send_and_receive_command "BGSAVE" connection);;

let lastsave connection =
    (* LASTSAVE *)
    match send_and_receive_command_safely "LASTSAVE" connection with
        Integer(x) -> float_of_int x |
        LargeInteger(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let shutdown connection =
    (* SHUTDOWN *)
    Connection.send_text "SHUTDOWN" connection;
    try
        match receive_answer connection with
            Status(x) -> failwith x | 
            _ -> failwith "Did not recognize what I got back"
    with End_of_file -> ();;

let bgrewriteaof connection =
    (* BGREWRITEAOF *)
    handle_special_status
        "Background append only file rewriting started"
        (send_and_receive_command "BGREWRITEAOF" connection);;

(* Remote server control commands *)

module Info =
    struct
        type t = {fields: string list; values: (string, string) Hashtbl.t;}
        let tokenizer text = 
            let line_spliter line =
                let colon_index = String.index line ':' in
                let key = String.sub line 0 colon_index in
                let value = String.sub line (colon_index + 1) ((String.length line) - 1 -colon_index) in
                    (key, value)
            in
                List.map line_spliter
                    (Str.split (Str.regexp "\r\n") text)
        let create text =
            let values = Hashtbl.create 10
            in
            let loader (key, value) = begin
                    Hashtbl.add values key value;
                    key
                end
            in
            let fields = List.map loader (tokenizer text)
            in
                {fields=fields; values=values}
        let get info field =
            Hashtbl.find info.values field
        let get_fields info =
            info.fields
    end;;
        
let info connection =
    (* INFO *)
    match send_and_receive_command_safely "INFO" connection with
        Bulk(x) -> Info.create (string_of_bulk_data x) |
        _ -> failwith "Did not recognize what I got back";;

let slaveof addr port connection =
    (* SLAVEOF *)
    handle_status
        (send_and_receive_command_safely
            (Printf.sprintf "SLAVEOF %s %d" addr port)
            connection);;
    
