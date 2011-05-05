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
  expect_status "PONG" (send connection "PING");
  true

(* QUIT, also should automatically close the connection *)
let quit connection =
  Connection.send_text connection "QUIT"

(* AUTH *)
let auth connection password =
  expect_success (send connection ("AUTH " ^ password))

(***************************************)
(* Commands operating on string values *)
(***************************************)

(* SET *)
let set connection key value =
  expect_success (send_multibulk connection ["SET"; key; value])

(* GET *)
let get connection key =
  expect_bulk (send_multibulk connection ["GET"; key])

(* GETSET *)
let getset connection key new_value =
  expect_bulk (send_multibulk connection ["GETSET"; key; new_value])

(* MGET *)
let mget connection keys = 
  expect_non_nil_multibulk (send_multibulk connection ("MGET" :: keys))

(* SETNX *)
let setnx connection key value =
  expect_bool (send_multibulk connection ["SETNX"; key; value])

(* SETEX *)
let setex connection key timeout value =
  let cmd = ["SETEX"; key; (string_of_int timeout); value] in
  expect_success (send_multibulk connection cmd)

(* MSET *)
let mset connection key_value_pairs =
  expect_success (send_multibulk connection ("MSET" :: (flatten key_value_pairs [])))

(* MSETNX *)
let msetnx connection key_value_pairs =
  expect_bool (send_multibulk connection ("MSETNX" :: (flatten key_value_pairs [])))

(* INCR *)
let incr connection key =
  expect_int (send_multibulk connection ["INCR"; key])

(* INCRBY *)
let incrby connection key value =
  expect_int (send_multibulk connection ["INCRBY"; key; string_of_int value])

(* DECR *)
let decr connection key =
  expect_int (send_multibulk connection ["DECR"; key])

(* DECRBY *)
let decrby connection key value =
  expect_int (send_multibulk connection ["DECRBY"; key; string_of_int value])

(* APPEND *)
let append connection key value =
  expect_int (send_multibulk connection ["APPEND"; key; value])

(* SUBSTR, note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let substr connection key start stop =
  let cmd = ["SUBSTR"; key; string_of_int start; string_of_int stop] in
  expect_bulk (send_multibulk connection cmd)

(* EXISTS *)
let exists connection key =
  expect_bool (send_multibulk connection ["EXISTS"; key])

(* DEL *)
let del connection keys = 
  expect_int (send_multibulk connection ("DEL" :: keys))

(* Exactly like "del", except you do not need to provide a list, 
   just one key. Not in spec *)
let del_one connection key =
  expect_bool (send_multibulk connection ["DEL"; key])

(* TYPE, unfortunately type is an ocaml keyword, 
   so it cannot be used as a function name *)
let value_type connection key =
  expect_type (send_multibulk connection ["TYPE"; key])

(***************************************)
(* Commands operating on the key space *)
(***************************************)

(* KEYS *)
let keys connection pattern =
  expect_multibulk_list (send_multibulk connection ["KEYS"; pattern])

(* RANDOMKEY *)
let randomkey connection =
  string_of_bulk_data (expect_bulk (send_multibulk connection ["RANDOMKEY"]))

(* RENAME *)
let rename connection oldkey newkey =
  expect_success (send_multibulk connection ["RENAME"; oldkey; newkey])

(* RENAMENX *)
let renamenx connection oldkey newkey =
  expect_bool (send_multibulk connection ["RENAMENX"; oldkey; newkey])

(* DBSIZE *)
let dbsize connection =
  expect_int (send_multibulk connection ["DBSIZE"])

(* EXPIRE *)
let expire connection key seconds = 
  expect_bool (send_multibulk connection ["EXPIRE"; key; string_of_int seconds])

(* EXPIREAT *)
let expireat connection key time =
  expect_bool (send_multibulk connection ["EXPIREAT"; key; Printf.sprintf "%.f" time])

(* TTL *)
let ttl connection key =
  expect_int (send_multibulk connection ["TTL"; key])

(*******************************)
(* Commands operating on lists *)
(*******************************)

(* RPUSH *)
let rpush connection key value =
  expect_int (send_multibulk connection ["RPUSH"; key; value])

(* LPUSH *)
let lpush connection key value =
  expect_int (send_multibulk connection ["LPUSH"; key; value])

(* LLEN *)
let llen connection key =
  expect_int (send_multibulk connection ["LLEN"; key])

(* LRANGE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let lrange connection key start stop =
  let cmd = ["LRANGE"; key; string_of_int start; string_of_int stop] in
  expect_non_nil_multibulk (send_multibulk connection cmd)

(* LTRIM, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let ltrim connection key start stop =
  let cmd = ["LTRIM"; key; string_of_int start; string_of_int stop] in
  expect_success (send_multibulk connection cmd)

(* LINDEX *)
let lindex connection key index =
  let cmd = ["LINDEX"; key; string_of_int index] in
  expect_bulk (send_multibulk connection cmd)

(* LSET *)
let lset connection key index value =
  let cmd = ["LSET"; key; string_of_int index; value]  in
  expect_success (send_multibulk connection cmd)

(* LREM *)
let lrem connection key count value =
  let cmd = ["LREM"; key; string_of_int count; value] in
  expect_int (send_multibulk connection cmd)

(* LPOP *)
let lpop connection key =
  expect_bulk (send_multibulk connection ["LPOP"; key])

(* RPOP *)
let rpop connection key =
  expect_bulk (send_multibulk connection ["RPOP"; key])

(* RPOPLPUSH *)
let rpoplpush connection src_key dest_key =
  expect_bulk (send_multibulk connection ["RPOPLPUSH"; src_key; dest_key])

let string_of_timeout = function
  | Seconds seconds -> string_of_int seconds
  | Wait       -> "0"
    
(* BLPOP, but for only one key *)
let blpop connection ?(timeout = Wait) key =
  let cmd = ["BLPOP"; key; string_of_timeout timeout] in
  expect_multibulk_kv (send_multibulk connection cmd)

(* BLPOP, but for many keys *)
let blpop_many connection ?(timeout = Wait) key_list =
  let cmd = ("BLPOP" :: key_list) @ [string_of_timeout timeout] in
  expect_multibulk_skv (send_multibulk connection cmd)
    
(* BRPOP, but for only one key *)
let brpop connection ?(timeout = Wait) key =
  let cmd = ["BRPOP"; key; string_of_timeout timeout] in
  expect_multibulk_kv (send_multibulk connection cmd)

(* BRPOP *)
let brpop_many connection ?(timeout = Wait) key_list =
  let cmd = "BRPOP" :: key_list @ [string_of_timeout timeout] in
  expect_multibulk_skv (send_multibulk connection cmd)

(******************************)
(* Commands operating on sets *)
(******************************)

(* SADD *)
let sadd connection key member =
  expect_bool (send_multibulk connection ["SADD"; key; member])

(* SREM *)
let srem connection key member =
  expect_bool (send_multibulk connection ["SREM"; key; member])

(* SPOP *)
let spop connection key =
  expect_bulk (send_multibulk connection ["SPOP"; key])

(* SMOVE *)
let smove connection srckey destkey member =
  expect_bool (send_multibulk connection ["SMOVE"; srckey; destkey; member])

(* SCARD *)
let scard connection key =
  expect_int (send_multibulk connection ["SCARD"; key])

(* SISMEMBER *)
let sismember connection key member =
  expect_bool (send_multibulk connection ["SISMEMBER"; key; member])

(* SMEMBERS *)
let smembers connection key =
  expect_non_nil_multibulk (send_multibulk connection ["SMEMBERS"; key])

(* SINTER *)
let sinter connection keys =
  expect_non_nil_multibulk (send_multibulk connection ("SINTER" :: keys))

(* SINTERSTORE *)
let sinterstore connection dstkey keys =
  expect_int (send_multibulk connection ("SINTERSTORE" :: dstkey :: keys))

(* SUNION *)
let sunion connection keys =
  expect_non_nil_multibulk (send_multibulk connection ("SUNION" :: keys))

(* SUNIONSTORE *)
let sunionstore connection dstkey keys =
  expect_int (send_multibulk connection ("SUNIONSTORE" :: dstkey :: keys))

(* SDIFF *)
let sdiff connection from_key keys =
  expect_non_nil_multibulk (send_multibulk connection ("SDIFF" :: from_key :: keys))

(* SDIFFSTORE *)
let sdiffstore connection dstkey from_key keys =
  let cmd = "SDIFFSTORE" :: dstkey :: from_key :: keys in
  expect_int (send_multibulk connection cmd)

(* SRANDMEMBER *)
let srandmember connection key =
  expect_bulk (send_multibulk connection ["SRANDMEMBER"; key])

(****************************************)
(* Multiple databases handling commands *)
(****************************************)

(* SELECT *)
let select connection index =
  expect_success (send_multibulk connection ["SELECT"; string_of_int index])

(* MOVE *)
let move connection key index =
  expect_bool (send_multibulk connection ["MOVE"; key; string_of_int index])

(* FLUSHDB *)
let flushdb connection =
  expect_success (send_multibulk connection ["FLUSHDB"])

(* FLUSHALL *)
let flushall connection =
  expect_success (send_multibulk connection ["FLUSHALL"])

(*************************************)
(* Commands operating on sorted sets *)
(*************************************)

(* ZADD *)
let zadd connection key score member =
  let cmd = ["ZADD"; key; string_of_float score; member] in
  expect_bool (send_multibulk connection cmd)

(* ZREM *)
let zrem connection key member =
  expect_bool (send_multibulk connection ["ZREM"; key; member])

(* ZRANGE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let zrange connection key start stop =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop] in
  expect_non_nil_multibulk (send_multibulk connection cmd)

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
let zrange_with_scores connection key start stop =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop; "WITHSCORES"] in
  score_transformer (expect_non_nil_multibulk (send_multibulk connection cmd))

(* ZREVRANGE, please note that the word 'end' is a keyword in ocaml, 
so it has been replaced by 'stop' *)
let zrevrange connection key start stop =
  let cmd = ["ZREVRANGE"; key; string_of_int start; string_of_int stop] in
  expect_non_nil_multibulk (send_multibulk connection cmd)

(* ZRANGE, but with the WITHSCORES option added on. *)
let zrevrange_with_scores connection key start stop =
  let cmd = ["ZREVRANGE"; key; string_of_int start; 
             string_of_int stop; "WITHSCORES"] in
  score_transformer (expect_non_nil_multibulk 
                       (send_multibulk connection cmd))

(* ZRANGEBYSCORE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let zrangebyscore connection ?(limit = Unlimited) key start stop =
  let limit = match limit with
    | Unlimited    -> []
    | Limit (x, y) -> ["LIMIT"; string_of_int x; string_of_int y]
  in
  let cmd = ["ZRANGEBYSCORE"; key; 
             string_of_float start;
             string_of_float stop] @ limit in
  expect_non_nil_multibulk (send_multibulk connection cmd)

(* ZINCRBY *)
let zincrby connection key increment member =
  let cmd = ["ZINCRBY"; key; string_of_float increment; member] in
  expect_float (send_multibulk connection cmd)

(* ZRANK *)
let zrank connection key member =
  expect_rank (send_multibulk connection ["ZRANK"; key; member])

(* ZREVRANK *)
let zrevrank connection key member =
  expect_rank (send_multibulk connection ["ZREVRANK"; key; member])

(* ZCARD *)
let zcard connection key =
  expect_int (send_multibulk connection ["ZCARD"; key])

(* ZSCORE *)
let zscore connection key member =
  expect_float (send_multibulk connection ["ZSCORE"; key; member])

(* ZREMRANGEBYRANK *)
let zremrangebyrank connection key start stop =
  let cmd = ["ZREMRANGEBYRANK"; key; string_of_int start; string_of_int stop] in
  expect_int (send_multibulk connection cmd)

(* ZREMRANGEBYSCORE *)
let zremrangebyscore connection key min max =
  let cmd = ["ZREMRANGEBYSCORE"; key; 
             string_of_float min; string_of_float max] in
  expect_int (send_multibulk connection cmd)

let keylen l = string_of_int (List.length l)

let string_of_aggregate a = 
  let s = match a with 
    | Sum -> "SUM" 
    | Min -> "MIN" 
    | Max -> "MAX"
  in 
  ["AGGREGATE"; s] 

let zunioncmd cmd connection ?(aggregate = Sum) dstkey key_list =
  let cmd = [cmd; dstkey; keylen key_list] 
    @ key_list @ (string_of_aggregate aggregate) in
  expect_int (send_multibulk connection cmd)

let zunionstore = zunioncmd "ZUNIONSTORE"
let zinterstore = zunioncmd "ZINTERSTORE"

let zunioncmd_with_weights cmd connection ?(aggregate = Sum) dstkey key_list weight_list =
  if List.length key_list != List.length weight_list
  then raise (RedisInvalidArgumentError("Not as many weights were given as keys to " ^ cmd));
  let weights = List.map string_of_float weight_list in
  let cmd = [cmd; dstkey; keylen key_list] 
    @ key_list @ ("WEIGHTS" :: weights) @ (string_of_aggregate aggregate) in
  expect_int (send_multibulk connection cmd)

let zunionstore_with_weights = zunioncmd_with_weights "ZUNIONSTORE"                   
let zinterstore_with_weights = zunioncmd_with_weights "ZINTERSTORE"

(********************************)
(* Commands operating on hashes *)
(********************************)

(* HSET *)
let hset connection key field value =
  expect_bool (send_multibulk connection ["HSET"; key; field; value])

(* HDEL *)
let hdel connection key field =
  expect_bool (send_multibulk connection ["HDEL"; key; field])
    
(* HGET *)
let hget connection key field =
  expect_bulk (send_multibulk connection ["HGET"; key; field])

(* HMGET *)
let hmget connection key field_list =
  let cmd = "HMGET" :: key :: field_list in
  expect_multibulk (send_multibulk connection cmd)

(* HMSET *)
let hmset connection key field_value_pairs =
  let f rest el = (fst el) :: (snd el) :: rest in
  let values = List.fold_left f [] field_value_pairs in
  expect_success (send_multibulk connection ("HMSET" :: key :: values))

(* HINCRBY *)
let hincrby connection key field value =
  let cmd = ["HINCRBY"; key; field; string_of_int value] in
  expect_int (send_multibulk connection cmd)

(* HEXISTS *)
let hexists connection key field =
  expect_bool (send_multibulk connection ["HEXISTS"; key; field])

(* HLEN *)
let hlen connection key =
  expect_int (send_multibulk connection ["HLEN"; key])

(* HKEYS *)
let hkeys connection key =
  expect_multibulk_list (send_multibulk connection ["HKEYS"; key])

(* HVALS *)
let hvals connection key =
  expect_multibulk_list (send_multibulk connection ["HVALS"; key])

(* HGETALL *)
let hgetall connection key =
  let rec collate_pairs elems out =
    match elems with
      | []             -> List.rev out
      | f :: v :: rest -> collate_pairs rest ((f,v) :: out) 
      | _              -> failwith "Did not provide a pair of field-values"
  in
  let reply : response = send_multibulk connection ["HGETALL"; key] in
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
let sort connection
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(get = NoPattern)
    ?(order = Asc)
    ?(alpha = NonAlpha) 
    key =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    "SORT " ^ key ^ pattern ^ limit ^ (parse_get_arg get) ^ order ^ alpha in
  expect_non_nil_multibulk (send connection command)

(* SORT, for multiple gets *)
let sort_get_many connection 
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(order = Asc)
    ?(alpha = NonAlpha) 
    key get_patterns =
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
  collate_response patlen (expect_non_nil_multibulk (send connection command))

(* SORT, with the STORE keyword *)
let sort_and_store connection
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(order = Asc)
    ?(alpha = NonAlpha) 
    key get_patterns dest_key =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    let f rest n = rest ^ " GET " ^ n in
    let get = List.fold_left f "" get_patterns in
    let store = " STORE " ^ dest_key in
    "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha ^ store in
  expect_int (send connection command)

(********************************)
(* Persistence control commands *)
(********************************)

(* SAVE *)
let save connection =
  expect_success (send connection "SAVE")

(* BGSAVE *)
let bgsave connection =
  expect_status "Background saving started" (send connection "BGSAVE")

(* LASTSAVE *)
let lastsave connection =
  expect_large_int (send connection "LASTSAVE")

(* SHUTDOWN *)
let shutdown connection =
  Connection.send_text connection "SHUTDOWN";
  try
    match receive_answer connection with
      | Status x -> failwith x 
      | _        -> failwith "Did not recognize what I got back"
  with End_of_file -> ()

(* BGREWRITEAOF *)
let bgrewriteaof connection =
  expect_status 
    "Background append only file rewriting started"
    (send connection "BGREWRITEAOF")

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
  let result = expect_bulk (send connection "INFO") in
  Info.create (string_of_bulk_data result)

(* SLAVEOF *)
let slaveof connection addr port =
  expect_success (send connection (Printf.sprintf "SLAVEOF %s %d" addr port))

