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

module Helpers =
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
            (* Will send a given list of tokens to send in multibulk using the unified request protocol. *)
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

        let send_multibulk_and_receive_command_safely tokens connection =
            (* Will send out the command in the same way as "send_multibulk_command" but will catch and fail on any errors. *)
            handle_error (send_multibulk_command tokens connection);;

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
