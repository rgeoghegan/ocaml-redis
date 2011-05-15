(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Utility code used mostly internally to the library. *)

type timeout = 
  | Seconds of int
  | Wait

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

(* This is an internal type used to transform 
   what the Redis server can handle into ocaml types. *)

type response = 
  | Status of string 
  | Integer of int 
  | LargeInteger of int64
  | Bulk of string option
  | MultiBulk of string option list option
  | Error of string

(* Different printing functions for the above types *)

let string_of_response r =
  let bulk_printer = function
    | Some x -> Printf.sprintf "String(%S)" x
    | None   -> "Nil"
  in
  let multi_bulk_printer l = 
    String.concat "; " (List.map bulk_printer l)
  in  match r with
    | Status x           -> Printf.sprintf "Status(%S)" x 
    | Integer x          -> Printf.sprintf "Integer(%d)" x
    | LargeInteger x     -> Printf.sprintf "LargeInteger(%Ld)" x
    | Error x            -> Printf.sprintf "Error(%S)" x
    | Bulk x             -> Printf.sprintf "Bulk(%s)" (bulk_printer x)
    | MultiBulk None     -> Printf.sprintf "MultiBulk(Nil)"
    | MultiBulk (Some x) -> Printf.sprintf "MultiBulk(%s)" (multi_bulk_printer x)

(* Simple function to print out a msg to stdout, should not be used in production code. *)
let debug fmt = 
  Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt

module Value = struct

  type t = 
    | Nil 
    | String 
    | List 
    | Set 
    | SortedSet

  let get = function 
    | Some x -> x
    | None   -> failwith "Value.get: unexpected None"

  let to_string = function
    | Nil       -> "Nil"
    | String    -> "String" 
    | List      -> "List" 
    | Set       -> "Set"
    | SortedSet -> "ZSet"

end
  
(* Continuation for pipelined receive *)

type 'a continuation = 
  | Expect_status of string
  | Expect_success 
  | Expect_bulk of ('a -> string option -> 'a)
  | Expect_multi of ('a -> string option list -> 'a)
  | Expect_bool of ('a -> bool -> 'a)
  | Expect_large_int of ('a -> int64 -> 'a)
  | Expect_int of ('a -> int -> 'a)
  | Expect_string of ('a -> string -> 'a)
  | Expect_float of ('a -> float -> 'a)
  | Expect_opt_float of ('a -> float option -> 'a)
  | Expect_type of ('a -> Value.t -> 'a)
  | Expect_list of ('a -> string list -> 'a)
  | Expect_kv_multi of ('a -> (string * string) option -> 'a)
  | Expect_rank of ('a -> int option -> 'a)
  | Collate of ('a -> (string * string) list -> 'a)
  | Collate_n of int * ('a -> string list list -> 'a)
  | Score_transformer of ('a -> (string * float) list -> 'a)

(* The Connection module handles some low level operations with the sockets *)
module Connection = struct
    
  type 'a t = {
    mutable pipeline : bool;
    in_ch : in_channel;
    out_ch : out_channel;
    in_buf : Buffer.t;
    out_buf : Buffer.t;
    mutable continuations : 'a continuation list;
  }

  let create addr port =
    let server = Unix.inet_addr_of_string addr in
    let in_ch, out_ch = Unix.open_connection (Unix.ADDR_INET (server, port)) in
    let pipeline = false
    and continuations = []
    and in_buf = Buffer.create 100
    and out_buf = Buffer.create 100 in
    { pipeline; in_ch; out_ch; in_buf; out_buf; continuations }
      
  (* Read arbitratry length string (hopefully quite short) from current pos in in_chan until \r\n *)
  let read_string conn = 
    let s = input_line conn.in_ch in
    let n = String.length s - 1 in
    if s.[n] = '\r' then 
      String.sub s 0 n
    else 
      s

  (* Read length caracters from in channel and output them as string *)
  let read_fixed_string conn length =
    Buffer.clear conn.in_buf;
    Buffer.add_channel conn.in_buf conn.in_ch (length + 2); (* + \r\n *)
    Buffer.sub conn.in_buf 0 length

  (* Retrieve one character from the connection *)
  let get_one_char conn = 
    input_char conn.in_ch

  let enable_pipeline conn = 
    conn.pipeline <- true;
    Buffer.clear conn.out_buf
        
  (* Add text to the pipeline *)
  let pipeline_cmd conn text =
    Buffer.add_string conn.out_buf text;
    Buffer.add_string conn.out_buf "\r\n"
        
  let flush_pipeline conn =
    Buffer.output_buffer conn.out_ch conn.out_buf;
    flush conn.out_ch;
    conn.pipeline <- false;
    Buffer.clear conn.out_buf

end

module Helpers = struct

  (* Gets the data from a '$x\r\nx\r\n' response, 
     once the first '$' has already been popped off *)
  let get_bulk conn = function
    |  0 -> Bulk (Some (Connection.read_fixed_string conn 0))
    | -1 -> Bulk None
    |  n -> Bulk (Some (Connection.read_fixed_string conn n))

  (* Parse list structure, with first '*' already popped off *)
  let get_multi_bulk conn size =
    match size with
      |  0 -> MultiBulk (Some [])
      | -1 -> MultiBulk None
      |  n -> 
        let acc = ref [] in
        for i = 0 to n - 1 do
          let bulk = match input_char conn.Connection.in_ch with
            | '$' -> begin
              match get_bulk conn (int_of_string (Connection.read_string conn)) with
                | Bulk x -> x
                | _      -> failwith "get_multi_bulk: bulk expected"
            end
            |  c  -> 
              failwith ("get_multi_bulk: unexpected char " ^ (Char.escaped c))
          in 
          acc := bulk :: !acc
        done;
        MultiBulk (Some (List.rev !acc))

  (* Expecting an integer response, will return the right type depending on the size *)
  let parse_integer_response response =
    try
      Integer (int_of_string response)
    with Failure "int_of_string" ->
      LargeInteger (Int64.of_string response)

  (* Filters out any errors and raises them.
     Raises RedisServerError with the error message 
     if getting an explicit error from the server ("-...")
     or a RedisServerError with "Could not decipher response" 
     for an unrecognised response type. *)
  let filter_error = function
    | Error e -> 
      raise (RedisServerError e)
    | x -> x

  (* Get answer back from redis and cast it to the right type *)
  let recv conn =
    Buffer.clear conn.Connection.in_buf;
    let c = Connection.get_one_char conn in
    let reply = match c with
      | '+' -> Status (Connection.read_string conn)
      | ':' -> parse_integer_response (Connection.read_string conn)
      | '$' -> get_bulk conn (int_of_string (Connection.read_string conn))
      | '*' -> get_multi_bulk conn (int_of_string (Connection.read_string conn))
      | '-' -> Error (Connection.read_string conn)
      |  c  -> failwith ("recv: unexpected char " ^ (Char.escaped c))
    in
    filter_error reply

  let just_send conn command =
    Connection.pipeline_cmd conn command;
    Connection.flush_pipeline conn

  (* Send command, and receive the result casted to the right type,
     catch and fail on errors *)
  let send conn command =
    just_send conn command;
    filter_error (recv conn)

  let pipe_send conn command k =
    let open Connection in
    pipeline_cmd conn command;
    conn.continuations <- k :: conn.continuations;
    ()

  (* Will send a given list of tokens to send in list
     using the unified request protocol. *)          
  let send_multi conn tokens =
    let open Connection in
    let token_length = (string_of_int (List.length tokens)) in
    let rec send_in_tokens tokens_left =
      match tokens_left with
        | [] -> ()
        | h :: t -> 
          pipeline_cmd conn ("$" ^ (string_of_int (String.length h)));
          pipeline_cmd conn h;
          send_in_tokens t
    in
    pipeline_cmd conn ("*" ^ token_length);
    send_in_tokens tokens;
    flush_pipeline conn;
    let reply = recv conn in
    filter_error reply

  let pipe_send_multi conn tokens k =
    let open Connection in
    let token_length = (string_of_int (List.length tokens)) in
    let rec send_in_tokens tokens_left =
      match tokens_left with
        | [] -> ()
        | h :: t -> 
          pipeline_cmd conn ("$" ^ (string_of_int (String.length h)));
          pipeline_cmd conn h;
          send_in_tokens t
    in
    pipeline_cmd conn ("*" ^ token_length);
    send_in_tokens tokens;
    conn.continuations <- k :: conn.continuations;
    ()

  let fail_with_reply name reply = 
    failwith (name ^ ": Unexpected " ^ (string_of_response reply))

  (* For status replies, does error checking and display *)
  let expect_status special_status reply =
    match filter_error reply with
      | Status x when x = special_status -> ()
      | x -> fail_with_reply "expect_status" x
        
  (* The most common case of expect_status is the "OK" status *)
  let expect_success = expect_status "OK"

  (* For integer replies, does error checking and casting to boolean *)
  let expect_bool reply =
    match filter_error reply with
      | Integer 0 -> false
      | Integer 1 -> true
      | x         ->  fail_with_reply "expect_bool" x

  let expect_int = function
    | Integer x -> x
    | x         -> fail_with_reply "expect_int" x

  let expect_large_int = function
    | Integer x      -> Int64.of_int x
    | LargeInteger x -> x
    | x              -> fail_with_reply "expect_large_int" x

  let expect_rank = function
    | Integer x -> Some x
    | Bulk None -> None
    | x         -> fail_with_reply "expect_rank" x

  let expect_bulk = function
    | Bulk x -> x
    | x      -> fail_with_reply "expect_bulk" x

  let rec expect_string = function
    | Bulk (Some x) -> x
    | x             -> fail_with_reply "expect_string" x

  let expect_list = function
      | MultiBulk (Some l) as x -> 
        let f = function
          | Some x -> x
          | None   -> fail_with_reply "expect_list" x
        in
        List.map f l
      | x -> 
        fail_with_reply "expect_list" x

  let expect_multi = function
    | MultiBulk (Some l) -> l
    | x                  -> fail_with_reply "expect_multi" x

  let expect_kv_multi = function
    | MultiBulk Some [Some k; Some v] -> Some (k, v)
    | MultiBulk None                  -> None
    | x                               -> fail_with_reply "expect_kv_multi" x

  (* For list replies that should be floating point numbers, 
     does error checking and casts to float *)
  let expect_float reply =
    match filter_error reply with
      | Bulk (Some x) -> begin
        try
          float_of_string x
        with Failure "float_of_string" ->
          failwith (Printf.sprintf "%S is not a floating point number" x)
      end
      | x -> fail_with_reply "expect_float" x

  let expect_opt_float reply =
    match filter_error reply with
      | Bulk (Some x) -> begin
        try
          Some (float_of_string x)
        with Failure "float_of_string" ->
          failwith (Printf.sprintf "%S is not a floating point number" x)
      end
      | Bulk None -> None
      | x         -> fail_with_reply "expect_opt_float" x

  let filter_error = function
    | Error e -> 
      raise (RedisServerError e)
    | x -> x

  let rec flatten pairs result =
    match pairs with
      | (key, value) :: t -> flatten t (key :: value :: result)
      | []                -> result

  let expect_type = function
    | Status "string" -> Value.String
    | Status "set"    -> Value.Set
    | Status "zset"   -> Value.SortedSet
    | Status "list"   -> Value.List
    | Status "none"   -> Value.Nil
    | x               -> fail_with_reply "expect_type" x


  (* Takes a list of [v_1; s_1; v_2; s_2; ...; v_n; s_n] and
     collates it into a list of pairs [(v_1, s_1); (v_2, s_2); ... ;
     (v_n, s_n)] *)

  let collate f l = 
    let rec collate' f l acc =
      match l with
        | []            -> List.rev acc
        | h1 :: h2 :: t -> collate' f t ((h1, f h2) :: acc) 
        | _             -> failwith "Did not provide a pair of field-values"
    in
    collate' f l []

  let score_transformer = collate float_of_string 

  (* Collates the returned list into a series of lists matching the 'GET' parameter *)
  let collate_n count responses =
    let rec iter n l acc1 acc2 =
      match (n, l) with
        | (0, _)      -> iter count l [] ((List.rev acc1) :: acc2)
        | (count, []) -> List.rev acc2
        | (_, h :: t) -> iter (n - 1) t (h :: acc1) acc2
    in
    iter count responses [] []

  module Pipeline = struct

    let enable = Connection.enable_pipeline
        
    let receive conn state = 
      let f state k = 
        let reply = recv conn in
        match k with 
          | Expect_status status -> expect_status status reply; state
          | Expect_success -> expect_success reply; state
          | Expect_bulk f -> f state (expect_bulk reply)
          | Expect_multi f -> f state (expect_multi reply)
          | Expect_bool f -> f state (expect_bool reply)
          | Expect_large_int f -> f state (expect_large_int reply)
          | Expect_int f -> f state (expect_int reply)
          | Expect_string f -> f state (expect_string reply)
          | Expect_float f -> f state (expect_float reply)
          | Expect_opt_float f -> f state (expect_opt_float reply)
          | Expect_type f -> f state (expect_type reply)
          | Expect_list f -> f state (expect_list reply)
          | Expect_kv_multi f -> f state (expect_kv_multi reply)
          | Expect_rank f -> f state (expect_rank reply)
          | Collate f -> f state (collate (fun x -> x ) (expect_list reply))
          | Collate_n (n, f) -> f state (collate_n n (expect_list reply))
          | Score_transformer f -> f state (score_transformer (expect_list reply))
      in
      Connection.flush_pipeline conn;
      let state = List.fold_left f state (List.rev conn.Connection.continuations) in
      conn.Connection.continuations <- [];
      state

  end

end
