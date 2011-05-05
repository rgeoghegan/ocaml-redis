(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Main library file. *)

include Redis_common
open Helpers

(* From a string of the address, and a port as an int, 
   gets an input and output file discriptor *)
let create_connection ?(addr = "127.0.0.1") ?(port = 6379) () =
  Connection.create addr port

(***********************)
(* Connection handling *)
(***********************)

(* PING *)
let ping connection =
  expect_status "PONG" (send "PING" connection);
  true

(* QUIT, also should automatically close the connection *)
let quit connection =
  Connection.send_text "QUIT" connection

(* AUTH *)
let auth password connection =
  expect_success (send ("AUTH " ^ password) connection)

(***************************************)
(* Commands operating on string values *)
(***************************************)

(* SET *)
let set key value connection =
  expect_success (send_multibulk ["SET"; key; value] connection)

(* GET *)
let get key connection =
  expect_bulk (send_multibulk ["GET"; key] connection)

(* GETSET *)
let getset key new_value connection =
  expect_bulk (send_multibulk ["GETSET"; key; new_value] connection)

(* MGET *)
let mget keys connection = 
  expect_non_nil_multibulk (send_multibulk ("MGET" :: keys) connection)

(* SETNX *)
let setnx key value connection =
  expect_bool (send_multibulk ["SETNX"; key; value] connection)

(* SETEX *)
let setex key timeout value connection =
  let cmd = ["SETEX"; key; (string_of_int timeout); value] in
  expect_success (send_multibulk cmd connection)

(* MSET *)
let mset key_value_pairs connection =
  expect_success (send_multibulk ("MSET" :: (flatten key_value_pairs [])) connection)

(* MSETNX *)
let msetnx key_value_pairs connection =
  expect_bool (send_multibulk ( "MSETNX" :: (flatten key_value_pairs [])) connection)

(* INCR *)
let incr key connection =
  expect_int (send_multibulk ["INCR"; key] connection)

(* INCRBY *)
let incrby key value connection =
  expect_int (send_multibulk ["INCRBY"; key; string_of_int value] connection)

(* DECR *)
let decr key connection =
  expect_int (send_multibulk ["DECR"; key] connection)

(* DECRBY *)
let decrby key value connection =
  expect_int (send_multibulk ["DECRBY"; key; string_of_int value] connection)

(* APPEND *)
let append key value connection =
  expect_int (send_multibulk ["APPEND"; key; value] connection)

(* SUBSTR, note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let substr key start stop connection =
  let cmd = ["SUBSTR"; key; string_of_int start; string_of_int stop] in
  expect_bulk (send_multibulk cmd connection)

(* EXISTS *)
let exists key connection =
  expect_bool (send_multibulk ["EXISTS"; key] connection)

(* DEL *)
let del keys connection =
  expect_int (send_multibulk ("DEL" :: keys) connection)

(* Exactly like "del", except you do not need to provide a list, 
   just one key. Not in spec *)
let del_one key connection =
  expect_bool (send_multibulk ["DEL"; key] connection)

(* TYPE, unfortunately type is an ocaml keyword, 
   so it cannot be used as a function name *)
let value_type key connection =
  expect_type (send_multibulk ["TYPE"; key] connection)

(***************************************)
(* Commands operating on the key space *)
(***************************************)

(* KEYS *)
let keys pattern connection =
  expect_multibulk_list (send_multibulk ["KEYS"; pattern] connection)

(* RANDOMKEY *)
let randomkey connection =
  string_of_bulk_data (expect_bulk (send_multibulk ["RANDOMKEY"] connection))

(* RENAME *)
let rename oldkey newkey connection =
  expect_success (send_multibulk ["RENAME"; oldkey; newkey] connection)

(* RENAMENX *)
let renamenx oldkey newkey connection =
  expect_bool (send_multibulk ["RENAMENX"; oldkey; newkey] connection)

(* DBSIZE *)
let dbsize connection =
  expect_int (send_multibulk ["DBSIZE"] connection)

(* EXPIRE *)
let expire key seconds connection = 
  expect_bool (send_multibulk ["EXPIRE"; key; string_of_int seconds] connection)

(* EXPIREAT *)
let expireat key time connection =
  expect_bool (send_multibulk ["EXPIREAT"; key; Printf.sprintf "%.f" time] connection)

(* TTL *)
let ttl key connection =
  expect_int (send_multibulk ["TTL"; key] connection)

(*******************************)
(* Commands operating on lists *)
(*******************************)

(* RPUSH *)
let rpush key value connection =
  expect_int (send_multibulk ["RPUSH"; key; value] connection)

(* LPUSH *)
let lpush key value connection =
  expect_int (send_multibulk ["LPUSH"; key; value] connection)

(* LLEN *)
let llen key connection =
  expect_int (send_multibulk ["LLEN"; key] connection)

(* LRANGE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let lrange key start stop connection =
  let cmd = ["LRANGE"; key; string_of_int start; string_of_int stop] in
  expect_non_nil_multibulk(send_multibulk cmd connection)

(* LTRIM, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let ltrim key start stop connection =
  let cmd = ["LTRIM"; key; string_of_int start; string_of_int stop] in
  expect_success (send_multibulk cmd connection)

(* LINDEX *)
let lindex key index connection =
  let cmd = ["LINDEX"; key; string_of_int index] in
  expect_bulk (send_multibulk cmd connection)

(* LSET *)
let lset key index value connection =
  let cmd = ["LSET"; key; string_of_int index; value]  in
  expect_success (send_multibulk cmd connection)

(* LREM *)
let lrem key count value connection =
  let cmd = ["LREM"; key; string_of_int count; value] in
  expect_int (send_multibulk cmd connection)

(* LPOP *)
let lpop key connection =
  expect_bulk (send_multibulk ["LPOP"; key] connection)

(* RPOP *)
let rpop key connection =
  expect_bulk (send_multibulk ["RPOP"; key] connection)

(* RPOPLPUSH *)
let rpoplpush src_key dest_key connection =
  expect_bulk (send_multibulk ["RPOPLPUSH"; src_key; dest_key] connection)

let string_of_timeout = function
  | Seconds seconds -> string_of_int seconds
  | Wait       -> "0"
    
(* BLPOP, but for only one key *)
let blpop key ?(timeout = Wait) connection =
  let cmd = ["BLPOP"; key; string_of_timeout timeout] in
  expect_multibulk_kv (send_multibulk cmd connection)

(* BLPOP, but for many keys *)
let blpop_many key_list ?(timeout = Wait) connection =
  let cmd = ("BLPOP" :: key_list) @ [string_of_timeout timeout] in
  expect_multibulk_skv (send_multibulk cmd connection)
    
(* BRPOP, but for only one key *)
let brpop key ?(timeout = Wait) connection =
  let cmd = ["BRPOP"; key; string_of_timeout timeout] in
  expect_multibulk_kv (send_multibulk cmd connection)

(* BRPOP *)
let brpop_many key_list ?(timeout = Wait) connection =
  let cmd = "BRPOP" :: key_list @ [string_of_timeout timeout] in
  expect_multibulk_skv (send_multibulk cmd connection)

(******************************)
(* Commands operating on sets *)
(******************************)

(* SADD *)
let sadd key member connection =
  expect_bool (send_multibulk ["SADD"; key; member] connection)

(* SREM *)
let srem key member connection =
  expect_bool (send_multibulk ["SREM"; key; member] connection)

(* SPOP *)
let spop key connection =
  expect_bulk (send_multibulk ["SPOP"; key] connection)

(* SMOVE *)
let smove srckey destkey member connection =
  expect_bool (send_multibulk ["SMOVE"; srckey; destkey; member] connection)

(* SCARD *)
let scard key connection =
  expect_int (send_multibulk ["SCARD"; key] connection)

(* SISMEMBER *)
let sismember key member connection =
  expect_bool (send_multibulk ["SISMEMBER"; key; member] connection)

(* SMEMBERS *)
let smembers key connection =
  expect_non_nil_multibulk (send_multibulk ["SMEMBERS"; key] connection)

(* SINTER *)
let sinter keys connection =
  expect_non_nil_multibulk (send_multibulk ("SINTER" :: keys) connection)

(* SINTERSTORE *)
let sinterstore dstkey keys connection =
  expect_int (send_multibulk ("SINTERSTORE" :: dstkey :: keys) connection)

(* SUNION *)
let sunion keys connection =
  expect_non_nil_multibulk (send_multibulk ("SUNION" :: keys) connection)

(* SUNIONSTORE *)
let sunionstore dstkey keys connection =
  expect_int (send_multibulk ("SUNIONSTORE" :: dstkey :: keys) connection)

(* SDIFF *)
let sdiff from_key keys connection =
  expect_non_nil_multibulk (send_multibulk ("SDIFF" :: from_key :: keys) connection)

(* SDIFFSTORE *)
let sdiffstore dstkey from_key keys connection =
  let cmd = "SDIFFSTORE" :: dstkey :: from_key :: keys in
  expect_int (send_multibulk cmd connection)

(* SRANDMEMBER *)
let srandmember key connection =
  expect_bulk (send_multibulk ["SRANDMEMBER"; key] connection)

(****************************************)
(* Multiple databases handling commands *)
(****************************************)

(* SELECT *)
let select index connection =
  expect_success (send_multibulk ["SELECT"; string_of_int index] connection)

(* MOVE *)
let move key index connection =
  expect_bool (send_multibulk ["MOVE"; key; string_of_int index] connection)

(* FLUSHDB *)
let flushdb connection =
  expect_success (send_multibulk ["FLUSHDB"] connection)

(* FLUSHALL *)
let flushall connection =
  expect_success (send_multibulk ["FLUSHALL"] connection)

(*************************************)
(* Commands operating on sorted sets *)
(*************************************)

(* ZADD *)
let zadd key score member connection =
  let cmd = ["ZADD"; key; string_of_float score; member] in
  expect_bool (send_multibulk cmd connection)

(* ZREM *)
let zrem key member connection =
  expect_bool (send_multibulk ["ZREM"; key; member] connection)

(* ZRANGE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let zrange key start stop connection =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop] in
  expect_non_nil_multibulk (send_multibulk cmd connection)

(* Takes a list of [v_1; s_1; v_2; s_2; ...; v_n; s_n] and
   collates it into a list of pairs [(v_1, s_1); (v_2, s_2); ... ; (v_n, s_n)] *)
let score_transformer value_and_scores_list =
  let rec value_iter l out =
    match l with
      | []     -> List.rev out
      | h :: t -> score_iter t h out
  and score_iter l value out =
    match l with
      | h :: t ->
        let score = float_of_string (string_of_bulk_data h) in
        value_iter t ((value, score) :: out) 
      | []     -> failwith ("Missing score for value " ^ (string_of_bulk_data value))
  in
  value_iter value_and_scores_list []

(* ZRANGE, but with the WITHSCORES option added on. *)
let zrange_with_scores key start stop connection =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop; "WITHSCORES"] in
  score_transformer (expect_non_nil_multibulk (send_multibulk cmd connection))

(* ZREVRANGE, please note that the word 'end' is a keyword in ocaml, 
so it has been replaced by 'stop' *)
let zrevrange key start stop connection =
  let cmd = ["ZREVRANGE"; key; string_of_int start; string_of_int stop] in
  expect_non_nil_multibulk (send_multibulk cmd connection)

(* ZRANGE, but with the WITHSCORES option added on. *)
let zrevrange_with_scores key start stop connection =
  let cmd = ["ZREVRANGE"; key; string_of_int start; 
             string_of_int stop; "WITHSCORES"] in
  score_transformer (expect_non_nil_multibulk 
                       (send_multibulk cmd connection))

(* ZRANGEBYSCORE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let zrangebyscore key start stop ?(limit = Unlimited) connection =
  let limit = match limit with
    | Unlimited    -> []
    | Limit (x, y) -> ["LIMIT"; string_of_int x; string_of_int y]
  in
  let cmd = ["ZRANGEBYSCORE"; key; 
             string_of_float start;
             string_of_float stop] @ limit in
  expect_non_nil_multibulk (send_multibulk cmd connection)

(* ZINCRBY *)
let zincrby key increment member connection =
  let cmd = ["ZINCRBY"; key; string_of_float increment; member] in
  expect_float (send_multibulk cmd connection)

(* ZRANK *)
let zrank key member connection =
  expect_rank (send_multibulk ["ZRANK"; key; member] connection)

(* ZREVRANK *)
let zrevrank key member connection =
  expect_rank (send_multibulk ["ZREVRANK"; key; member] connection)

(* ZCARD *)
let zcard key connection =
  expect_int (send_multibulk ["ZCARD"; key] connection)

(* ZSCORE *)
let zscore key member connection =
  expect_float (send_multibulk ["ZSCORE"; key; member] connection)

(* ZREMRANGEBYRANK *)
let zremrangebyrank key start stop connection =
  let cmd = ["ZREMRANGEBYRANK"; key; string_of_int start; string_of_int stop] in
  expect_int (send_multibulk cmd connection)

(* ZREMRANGEBYSCORE *)
let zremrangebyscore key min max connection =
  let cmd = ["ZREMRANGEBYSCORE"; key; 
             string_of_float min; string_of_float max] in
  expect_int (send_multibulk cmd connection)

let keylen l = string_of_int (List.length l)

let string_of_aggregate a = 
  let s = match a with 
    | Sum -> "SUM" 
    | Min -> "MIN" 
    | Max -> "MAX"
  in 
  ["AGGREGATE"; s] 

let zunioncmd cmd dstkey key_list ?(aggregate = Sum) connection =
  let cmd = [cmd; dstkey; keylen key_list] 
    @ key_list @ (string_of_aggregate aggregate) in
  expect_int (send_multibulk cmd connection)

let zunionstore = zunioncmd "ZUNIONSTORE"
let zinterstore = zunioncmd "ZINTERSTORE"

let zunioncmd_with_weights cmd dstkey key_list weight_list ?(aggregate = Sum) connection =
  if List.length key_list != List.length weight_list
  then raise (RedisInvalidArgumentError("Not as many weights were given as keys to " ^ cmd));
  let weights = List.map string_of_float weight_list in
  let cmd = [cmd; dstkey; keylen key_list] 
    @ key_list @ ("WEIGHTS" :: weights) @ (string_of_aggregate aggregate) in
  expect_int (send_multibulk cmd connection)

let zunionstore_with_weights = zunioncmd_with_weights "ZUNIONSTORE"                   
let zinterstore_with_weights = zunioncmd_with_weights "ZINTERSTORE"

(********************************)
(* Commands operating on hashes *)
(********************************)

(* HSET *)
let hset key field value connection =
  expect_bool (send_multibulk ["HSET"; key; field; value] connection)

(* HDEL *)
let hdel key field connection =
  expect_bool (send_multibulk ["HDEL"; key; field] connection)
    
(* HGET *)
let hget key field connection =
  expect_bulk (send_multibulk ["HGET"; key; field] connection)

(* HMGET *)
let hmget key field_list connection =
  let cmd = "HMGET" :: key :: field_list in
  expect_multibulk (send_multibulk cmd connection)

(* HMSET *)
let hmset key field_value_pairs connection =
  let f rest el = (fst el) :: (snd el) :: rest in
  let values = List.fold_left f [] field_value_pairs in
  expect_success (send_multibulk ("HMSET" :: key :: values) connection)

(* HINCRBY *)
let hincrby key field value connection =
  let cmd = ["HINCRBY"; key; field; string_of_int value] in
  expect_int (send_multibulk cmd connection)

(* HEXISTS *)
let hexists key field connection =
  expect_bool (send_multibulk ["HEXISTS"; key; field] connection)

(* HLEN *)
let hlen key connection =
  expect_int (send_multibulk ["HLEN"; key] connection)

(* HKEYS *)
let hkeys key connection =
  expect_multibulk_list (send_multibulk ["HKEYS"; key] connection)

(* HVALS *)
let hvals key connection =
  expect_multibulk_list (send_multibulk ["HVALS"; key] connection)

(* HGETALL *)
let hgetall key connection =
  let rec collate_pairs elems out =
    match elems with
      | []             -> List.rev out
      | f :: v :: rest -> collate_pairs rest ((f,v) :: out) 
      | _              -> failwith "Did not provide a pair of field-values"
  in
  let reply : response = send_multibulk ["HGETALL"; key] connection in
  let result = expect_multibulk_list reply in 
  collate_pairs result []

(***********)
(* Sorting *)
(***********)

(* Some of the sort args need further parsing and are used across multiple functions. *)
let parse_sort_args pattern limit order alpha =
  let pattern = match pattern with
    | KeyPattern k        -> " BY " ^ k 
    | FieldPattern (k, f) -> Printf.sprintf " BY %s->%s" k f 
    | NoSort              -> " BY nosort"
    | NoPattern           -> ""
  in
  let limit = match limit with
    | Unlimited    -> ""
    | Limit (x, y) -> Printf.sprintf " LIMIT %d %d" x y
  in
  let order = match order with
    | Asc  -> ""
    | Desc -> " DESC"
  in
  let alpha = match alpha with
    | NonAlpha -> ""
    | Alpha    -> " ALPHA"
  in
  (pattern, limit, order, alpha)

(* The get argument needs specific parsing *)
let parse_get_arg = function
  | KeyPattern k        -> " GET " ^ k
  | FieldPattern (k, f) -> Printf.sprintf " GET %s->%s" k f
  | NoSort | NoPattern  -> ""

(* SORT *)
let sort key
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(get = NoPattern)
    ?(order = Asc)
    ?(alpha = NonAlpha)
    connection =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    "SORT " ^ key ^ pattern ^ limit ^ (parse_get_arg get) ^ order ^ alpha in
  expect_non_nil_multibulk (send command connection)

(* SORT, for multiple gets *)
let sort_get_many key get_patterns
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(order = Asc)
    ?(alpha = NonAlpha)
    connection =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    let f rest n = rest ^ " GET " ^ n in
    let get = List.fold_left f "" get_patterns in
    "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha in
  (* Collates the returned list into a series of lists matching the 'GET' parameter *)
  let collate_response count responses =
    let rec iter x whats_left current_response all_responses =
      match (x, whats_left) with
        | (0, _) -> iter count whats_left [] ((List.rev current_response) :: all_responses)
        | (count, []) -> List.rev all_responses
        | (_, h::t) -> iter (x - 1) t (h :: current_response) all_responses
    in
    iter count responses [] []
  in
  let patlen = List.length get_patterns in
  collate_response patlen (expect_non_nil_multibulk (send command connection))

(* SORT, with the STORE keyword *)
let sort_and_store key get_patterns dest_key
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(order = Asc)
    ?(alpha = NonAlpha)
    connection =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    let f rest n = rest ^ " GET " ^ n in
    let get = List.fold_left f "" get_patterns in
    let store = " STORE " ^ dest_key in
    "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha ^ store in
  expect_int (send command connection)

(********************************)
(* Persistence control commands *)
(********************************)

(* SAVE *)
let save connection =
  expect_success (send "SAVE" connection)

(* BGSAVE *)
let bgsave connection =
  expect_status "Background saving started" (send "BGSAVE" connection)

(* LASTSAVE *)
let lastsave connection =
  expect_large_int (send "LASTSAVE" connection)

(* SHUTDOWN *)
let shutdown connection =
  Connection.send_text "SHUTDOWN" connection;
  try
    match receive_answer connection with
      | Status x -> failwith x 
      | _        -> failwith "Did not recognize what I got back"
  with End_of_file -> ()

(* BGREWRITEAOF *)
let bgrewriteaof connection =
  expect_status 
    "Background append only file rewriting started"
    (send "BGREWRITEAOF" connection)

(* Remote server control commands *)

module Info = struct

  type t = {fields: string list; values: (string, string) Hashtbl.t}

  let tokenizer text = 
    let line_spliter line =
      let colon_index = String.index line ':' in
      let key = String.sub line 0 colon_index in
      let value = String.sub line (colon_index + 1) ((String.length line) - 1 -colon_index) in
      (key, value)
    in
    List.map line_spliter (Str.split (Str.regexp "\r\n") text)

  let create text =
    let values = Hashtbl.create 10 in
    let loader (key, value) = Hashtbl.add values key value; key in
    let fields = List.map loader (tokenizer text) in
    {fields; values}

  let get info field = Hashtbl.find info.values field
  let get_fields info = info.fields

end

(* INFO *)
let info connection =
  let result = expect_bulk (send "INFO" connection) in
  Info.create (string_of_bulk_data result)

(* SLAVEOF *)
let slaveof addr port connection =
  expect_success (send (Printf.sprintf "SLAVEOF %s %d" addr port) connection)

