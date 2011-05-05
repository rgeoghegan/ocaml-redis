(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Utility code used mostly internally to the library. *)

type redis_value_type = 
  | RedisString 
  | RedisNil 
  | RedisList 
  | RedisSet 
  | RedisZSet

type bulk_data = 
  | Nil 
  | String of string

type rank = 
  | NilRank 
  | Rank of int

type multibulk_data =
  | MultibulkNil 
  | MultibulkValue of bulk_data list

type timeout = 
  | Seconds of int
  | NoTimeout

type limit = 
  | Unlimited
  | Limit of int * int 

type aggregate = 
  | Min
  | Max
  | Sum

type redis_sort_pattern = 
  | KeyPattern of string 
  | FieldPattern of string * string 
  | NoSort 
  | NoPattern

type sort_order = 
  | Asc
  | Desc

type sort_alpha = 
  | Alpha
  | NonAlpha

exception RedisServerError of string
exception RedisNilError of string
exception RedisInvalidArgumentError of string

(* This is an internal type used to transform 
   what the Redis server can handle into ocaml types. *)

type response = 
  | Status of string 
  | Integer of int 
  | LargeInteger of float 
  | Bulk of bulk_data 
  | Multibulk of multibulk_data 
  | Error of string
  | Undecipherable 

(* Different printing functions for the above types *)

let string_of_bulk_data bd =
  match bd with
    | String x -> x 
    | Nil      -> raise (RedisNilError "Trying to extract string from none")

let int_of_rank r =
  match r with
    | Rank x  -> x 
    | NilRank -> raise (RedisNilError "Trying to extract string from none")

let string_of_response r =
  let bulk_printer x =
    match x with
      | String d -> Printf.sprintf "String(%S)" d
      | Nil      -> "Nil"
  in
  let rec multi_bulk_list_to_string l =
    match l with
      | [] -> ""
      | h :: t -> Printf.sprintf "; %s%s" (bulk_printer h) (multi_bulk_list_to_string t)
  in
  match r with
    | Status       x -> Printf.sprintf "Status(%S)" x 
    | Integer      x -> Printf.sprintf "Integer(%d)" x
    | LargeInteger x -> Printf.sprintf "LargeInteger(%.2f)" x
    | Error        x -> Printf.sprintf "Error(%S)" x
    | Bulk         x -> Printf.sprintf "Bulk(%s)" (bulk_printer x)
    | Multibulk x -> begin match x with
        | MultibulkNil      -> "MultibulkNil"
        | MultibulkValue [] -> Printf.sprintf "Multibulk([])"
        | MultibulkValue (h :: t) -> Printf.sprintf "Multibulk([%s%s])" (bulk_printer h) (multi_bulk_list_to_string t)
    end
    | Undecipherable -> "Undecipherable"

let string_of_redis_value_type vt =
  match vt with
    | RedisNil    -> "Nil" 
    | RedisString -> "String" 
    | RedisList   -> "List" 
    | RedisSet    -> "Set"
    | RedisZSet   -> "ZSet"

(* Simple function to print out a msg to stdout, should not be used in production code. *)
let debug_string comment value =
  Printf.printf "%s %S\n" comment value;
  flush stdout

(* The Connection module handles some low level operations with the sockets *)
module Connection = struct

  type t = in_channel * out_channel

  let create addr port =
    let server = Unix.inet_addr_of_string addr in
    let in_c, out_c = 
      Unix.open_connection (Unix.ADDR_INET (server, port)) in
    (in_c, out_c)

  (* Read arbitratry length string (hopefully quite short) from current pos in in_chan until \r\n *)
  let read_string (in_chan, _) =
    let rec iter out_buffer =
      match input_char in_chan with
        | '\r' -> begin match input_char in_chan with
            | '\n' -> Buffer.contents out_buffer 
            | x ->
              Buffer.add_char out_buffer '\r';
              Buffer.add_char out_buffer x;
              iter out_buffer
        end
        | x ->
          Buffer.add_char out_buffer x;
          iter out_buffer
    in
    iter (Buffer.create 100)

  (* Read length caracters from in channel and output them as string *)
  let read_fixed_string length (in_chan, _) =
    let out_buf = Buffer.create length in
    Buffer.add_channel out_buf in_chan length;
    Buffer.contents out_buf

  (* Send the given text out to the connection without flushing *)
  let send_text_straight text (_, out_chan) =
    output_string out_chan text;
    output_string out_chan "\r\n"

  (* Send the given text out to the connection *)
  let send_text text connection =
    let (_, out_chan) = connection in
    send_text_straight text connection;
    flush out_chan
      
  (* Retrieve one character from the connection *)
  let get_one_char (in_chan, out_chan) =
    input_char in_chan

  (* Manually flushes out the outgoing channel *)
  let flush_connection (in_chan, out_chan) =
    flush out_chan

end

module Helpers = struct

  (* Gets the data from a '$x\r\nx\r\n' response, 
     once the first '$' has already been popped off *)
  let get_bulk_data connection =
    let length = int_of_string (Connection.read_string connection) in
    if length == -1
    then Nil
    else let out_str = if length == 0
      then String("")
      else String(Connection.read_fixed_string length connection) in begin
        (* Exhausts the \r\n ending of a bulk data value *)
        List.iter (fun x -> assert(x == Connection.get_one_char connection)) ['\r'; '\n'];
        out_str
      end

  (* Parse multibulk data structure, with first '*' already popped off *)
  let get_multibulk_data size conn =
    let in_chan, _ = conn in
    let rec iter i data =
      if i == 0
      then Multibulk(MultibulkValue(List.rev data))
      else match input_char in_chan with
        | '$' -> iter (i - 1) ((get_bulk_data conn) :: data)
        | _   -> Undecipherable
    in
    match size with
      | -1           -> Multibulk MultibulkNil
      | 0            -> Multibulk (MultibulkValue [])
      | x when x > 0 -> iter size [] 
      | _            -> Undecipherable

  (* Expecting an integer response, will return the right type depending on the size *)
  let parse_integer_response response =
    try
      Integer (int_of_string response)
    with Failure "int_of_string" ->
      LargeInteger (float_of_string response)

  (* Filters out any errors and raises them.
     Raises RedisServerError with the error message 
     if getting an explicit error from the server ("-...")
     or a RedisServerError with "Could not decipher response" 
     for an unrecognised response type. *)
  let handle_error reply =
    match reply with
      | Error e        -> raise (RedisServerError e)
      | Undecipherable -> raise (RedisServerError "Could not decipher response") 
      | x              -> x

  (* Get answer back from redis and cast it to the right type *)
  let receive_answer connection =
    match (Connection.get_one_char connection) with
      | '+' -> Status (Connection.read_string connection)
      | ':' -> parse_integer_response (Connection.read_string connection)
      | '$' -> Bulk (get_bulk_data connection)
      | '*' -> get_multibulk_data (int_of_string (Connection.read_string connection)) connection
      | '-' -> Error (Connection.read_string connection)
      | _   -> ignore (Connection.read_string connection); Undecipherable

  (* Send command, and receive the result casted to the right type *)
  let send_recv_cmd command connection =
    Connection.send_text command connection;
    receive_answer connection

  (* Will send the command, much like send_and_receive_command, but will catch and failwith any errors *)
  let send_recv command connection =
    handle_error (send_recv_cmd command connection)

  (* Will send out the command, appended with the length of value, 
     and will then send out value. Also will catch and fail on any errors. 
     I.e., given 'foo' 'bar', will send "foo 3\r\nbar\r\n" *)
  let send_with_value_and_receive_command_safely command value connection =
    Connection.send_text_straight command connection;
    Connection.send_text value connection;
    handle_error (receive_answer connection) 

  (* Given a list of tokens, joins them with command *)
  let aggregate_command command tokens = 
    let joiner buf new_item = 
      Buffer.add_char buf ' ';
      Buffer.add_string buf new_item
    in
    match tokens with 
      | [] -> failwith "Need at least one key"
      | x ->
        let out_buffer = Buffer.create 256 in
        Buffer.add_string out_buffer command;
        List.iter (joiner out_buffer) x;
        Buffer.contents out_buffer

  (* Will send a given list of tokens to send in multibulk using the
  unified request protocol. *)
          
  let send_multibulk tokens connection =
    let token_length = (string_of_int (List.length tokens)) in
    let rec send_in_tokens tokens_left =
      match tokens_left with
        | [] -> ()
        | h :: t -> 
          Connection.send_text_straight ("$" ^ (string_of_int (String.length h))) connection;
          Connection.send_text_straight h connection;
          send_in_tokens t
    in
    Connection.send_text_straight ("*" ^ token_length) connection;
    send_in_tokens tokens;
    Connection.flush_connection connection;
    receive_answer connection

  (* Will send out the command in the same way as
     "send_multibulk_command" but will catch and fail on any errors. *)
  let send_recv_multibulk tokens connection =
    handle_error (send_multibulk tokens connection)

  (* For status replies, does error checking and display *)
  let handle_special_status special_status reply =
    match handle_error reply with
      | Status x when x = special_status -> ()
      | Status x -> failwith ("Received status(" ^ x ^ ")")
      | _        -> failwith "Did not recognize what I got back"
        
  (* The most common case of handle_special_status is the "OK" status *)
  let handle_status = handle_special_status "OK"

  let fail_with_reply name reply = 
    failwith (name ^ ": Unexpected " ^ (string_of_response reply))

  (* For integer replies, does error checking and casting to boolean *)
  let expect_bool reply =
    match handle_error reply with
      | Integer 0 -> false
      | Integer 1 -> true
      | x         ->  fail_with_reply "expect_bool" x

  let expect_int = function
    | Integer x -> x
    | x         -> fail_with_reply "expect_int" x

  let expect_large_int = function
    | Integer x      -> float_of_int x
    | LargeInteger x -> x
    | x              -> fail_with_reply "expect_large_int" x

  let expect_bulk = function
    | Bulk x -> x
    | x      -> fail_with_reply "expect_bulk" x

  let expect_rank = function
    | Integer x -> Rank x 
    | Bulk Nil  -> NilRank 
    | x         -> fail_with_reply "expect_rank" x

  let expect_multibulk_list = function
    | Multibulk (MultibulkValue l) -> List.map string_of_bulk_data l 
    | x                            -> fail_with_reply "expect_multibulk_list" x

  let expect_multibulk_kv = function
    | Multibulk (MultibulkValue [k; v]) -> v 
    | Multibulk MultibulkNil            -> Nil
    | x                                 -> fail_with_reply "expect_multibulk_kv" x

  let expect_multibulk_skv = function
    | Multibulk (MultibulkValue [k; v]) -> (string_of_bulk_data k), v 
    | Multibulk MultibulkNil            -> "", Nil
    | x                                 -> fail_with_reply "expect_multibulk_skv" x

  let expect_multibulk = function
    | Multibulk (MultibulkValue x) -> x 
    | x                            -> fail_with_reply "expect_multibulk" x

  (* Extracts bulk_data_list out of Multibulk replies, raises and error if a nil is found *)
  let expect_non_nil_multibulk = function
    | Multibulk MultibulkNil -> 
      raise (RedisNilError "Was not expecting MultibulkNil response.") 
    | Multibulk (MultibulkValue v) -> v 
    | x -> fail_with_reply "expect_non_nil_multibulk" x

  (* For bulk replies that should be floating point numbers, 
     does error checking and casts to float *)
  let expect_float reply =
    match handle_error reply with
      | Bulk (String x) -> begin
        try
          float_of_string x 
        with Failure "float_of_string" ->
          failwith (Printf.sprintf "%S is not a floating point number" x)
      end
      | Bulk Nil -> failwith "expect_float: Unexpected Bulk Nil"
      | x -> fail_with_reply "expect_float" x

  let rec flatten pairs result =
    match pairs with
      | (key, value) :: t -> flatten t (key :: value :: result)
      | []                -> result

  let expect_type = function
    | Status "string" -> RedisString
    | Status "set"    -> RedisSet
    | Status "zset"   -> RedisZSet
    | Status "list"   -> RedisList
    | Status "none"   -> RedisNil
    | x               -> fail_with_reply "expect_type" x

end
