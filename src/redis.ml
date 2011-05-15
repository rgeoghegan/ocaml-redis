(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Main library file. *)

include Redis_common
include Helpers

(* From a string of the address, and a port as an int, 
   gets an input and output file discriptor *)
let create_connection ?(addr = "127.0.0.1") ?(port = 6379) () =
  Connection.create addr port

(***********************)
(* Connection handling *)
(***********************)

(* PING *)
let ping connection =
  expect_status "PONG" (send connection "PING")

let pping connection = 
  let k = Expect_status "PONG" in
  pipe_send connection "PING" k

(* QUIT, also should automatically close the connection *)
let quit connection =
  expect_status "OK" (send connection "QUIT")

(* AUTH *)
let auth connection password =
  expect_success (send connection ("AUTH " ^ password))

let pauth connection password =
  let k = Expect_success in
  pipe_send connection ("AUTH " ^ password) k

(***************************************)
(* Commands operating on string values *)
(***************************************)

(* SET *)
let set connection key value =
  expect_success (send_multi connection ["SET"; key; value])

let pset connection key value =
  let k = Expect_success in
  pipe_send_multi connection ["SET"; key; value] k

(* GET *)
let get connection key =
  expect_bulk (send_multi connection ["GET"; key])

let pget connection key callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["GET"; key] k

(* GETSET *)
let getset connection key new_value =
  expect_bulk (send_multi connection ["GETSET"; key; new_value])

let pgetset connection key new_value callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["GETSET"; key; new_value] k

(* MGET *)
let mget connection keys = 
  expect_multi (send_multi connection ("MGET" :: keys))

let pmget connection keys callback = 
  let k = Expect_multi callback in
  pipe_send_multi connection ("MGET" :: keys) k

(* SETNX *)
let setnx connection key value =
  expect_bool (send_multi connection ["SETNX"; key; value])

let psetnx connection key value callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["SETNX"; key; value] k

(* SETEX *)
let setex connection key timeout value =
  let cmd = ["SETEX"; key; (string_of_int timeout); value] in
  expect_success (send_multi connection cmd)

let psetex connection key timeout value =
  let cmd = ["SETEX"; key; (string_of_int timeout); value] 
  and k = Expect_success in
  pipe_send_multi connection cmd k

(* MSET *)
let mset connection key_value_pairs =
  expect_success (send_multi connection ("MSET" :: (flatten key_value_pairs [])))

let pmset connection key_value_pairs =
  let k = Expect_success in
  pipe_send_multi connection ("MSET" :: (flatten key_value_pairs [])) k

(* MSETNX *)
let msetnx connection key_value_pairs =
  expect_bool (send_multi connection ("MSETNX" :: (flatten key_value_pairs [])))

let pmsetnx connection key_value_pairs callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ("MSETNX" :: (flatten key_value_pairs [])) k

(* INCR *)
let incr connection key =
  expect_large_int (send_multi connection ["INCR"; key])

let pincr connection key callback =
  let k = Expect_large_int callback in
  pipe_send_multi connection ["INCR"; key] k

(* INCRBY *)
let incrby connection key value =
  expect_large_int (send_multi connection ["INCRBY"; key; string_of_int value])

let pincrby connection key value callback =
  let k = Expect_large_int callback in 
  pipe_send_multi connection ["INCRBY"; key; string_of_int value] k

(* DECR *)
let decr connection key =
  expect_large_int (send_multi connection ["DECR"; key])

let pdecr connection key callback =
  let k = Expect_large_int callback in
  pipe_send_multi connection ["DECR"; key] k

(* DECRBY *)
let decrby connection key value =
  expect_large_int (send_multi connection ["DECRBY"; key; string_of_int value])

let pdecrby connection key value callback =
  let k = Expect_large_int callback in
  pipe_send_multi connection ["DECRBY"; key; string_of_int value] k

(* APPEND *)
let append connection key value =
  expect_int (send_multi connection ["APPEND"; key; value])

let pappend connection key value callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["APPEND"; key; value] k

(* GETRANGE, note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let getrange connection key start stop =
  let cmd = ["GETRANGE"; key; string_of_int start; string_of_int stop] in
  expect_string (send_multi connection cmd)

let pgetrange connection key start stop callback =
  let cmd = ["GETRANGE"; key; string_of_int start; string_of_int stop] 
  and k = Expect_string callback in
  pipe_send_multi connection cmd k

(* EXISTS *)
let exists connection key =
  expect_bool (send_multi connection ["EXISTS"; key])

let pexists connection key callback =
  let k = Expect_bool callback in 
  pipe_send_multi connection ["EXISTS"; key] k

(* DEL *)
let del connection keys = 
  expect_int (send_multi connection ("DEL" :: keys))

let pdel connection keys callback = 
  let k = Expect_int callback in
  pipe_send_multi connection ("DEL" :: keys) k

(* Exactly like "del", except you do not need to provide a list, 
   just one key. Not in spec *)
let del_one connection key =
  expect_bool (send_multi connection ["DEL"; key])

let pdel_one connection key callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["DEL"; key] k

(* TYPE, unfortunately type is an ocaml keyword, 
   so it cannot be used as a function name *)
let value_type connection key =
  expect_type (send_multi connection ["TYPE"; key])

let pvalue_type connection key callback =
  let k = Expect_type callback in
  pipe_send_multi connection ["TYPE"; key] k

(***************************************)
(* Commands operating on the key space *)
(***************************************)

(* KEYS *)
let keys connection pattern =
  expect_list (send_multi connection ["KEYS"; pattern])

let pkeys connection pattern callback =
  let k = Expect_list callback in 
  pipe_send_multi connection ["KEYS"; pattern] k

(* RANDOMKEY *)
let randomkey connection =
  expect_bulk (send_multi connection ["RANDOMKEY"])

let prandomkey connection callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["RANDOMKEY"] k

(* RENAME *)
let rename connection oldkey newkey =
  expect_success (send_multi connection ["RENAME"; oldkey; newkey])

let prename connection oldkey newkey =
  let k = Expect_success in
  pipe_send_multi connection ["RENAME"; oldkey; newkey] k

(* RENAMENX *)
let renamenx connection oldkey newkey =
  expect_bool (send_multi connection ["RENAMENX"; oldkey; newkey])

let prenamenx connection oldkey newkey callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["RENAMENX"; oldkey; newkey] k

(* DBSIZE *)
let dbsize connection =
  expect_int (send_multi connection ["DBSIZE"])

let pdbsize connection callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["DBSIZE"] k

(* EXPIRE *)
let expire connection key seconds = 
  expect_bool (send_multi connection ["EXPIRE"; key; string_of_int seconds])

let pexpire connection key seconds callback = 
  let k = Expect_bool callback in
  pipe_send_multi connection ["EXPIRE"; key; string_of_int seconds] k

(* EXPIREAT *)
let expireat connection key time =
  expect_bool (send_multi connection ["EXPIREAT"; key; Printf.sprintf "%.f" time])

let pexpireat connection key time callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["EXPIREAT"; key; Printf.sprintf "%.f" time] k

(* TTL *)
let ttl connection key =
  expect_int (send_multi connection ["TTL"; key])

let pttl connection key callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["TTL"; key] k

(*******************************)
(* Commands operating on lists *)
(*******************************)

(* RPUSH *)
let rpush connection key value =
  expect_int (send_multi connection ["RPUSH"; key; value])

let prpush connection key value callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["RPUSH"; key; value] k

(* LPUSH *)
let lpush connection key value =
  expect_int (send_multi connection ["LPUSH"; key; value])

let plpush connection key value callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["LPUSH"; key; value] k

(* LLEN *)
let llen connection key =
  expect_int (send_multi connection ["LLEN"; key])

let pllen connection key callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["LLEN"; key] k

(* LRANGE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let lrange connection key start stop =
  let cmd = ["LRANGE"; key; string_of_int start; string_of_int stop] in
  expect_list (send_multi connection cmd)

let plrange connection key start stop callback =
  let cmd = ["LRANGE"; key; string_of_int start; string_of_int stop] 
  and k = Expect_list callback in
  pipe_send_multi connection cmd k

(* LTRIM, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let ltrim connection key start stop =
  let cmd = ["LTRIM"; key; string_of_int start; string_of_int stop] in
  expect_success (send_multi connection cmd)

let pltrim connection key start stop =
  let cmd = ["LTRIM"; key; string_of_int start; string_of_int stop] 
  and k = Expect_success in
  pipe_send_multi connection cmd k

(* LINDEX *)
let lindex connection key index =
  let cmd = ["LINDEX"; key; string_of_int index] in
  expect_bulk (send_multi connection cmd)

let plindex connection key index callback =
  let cmd = ["LINDEX"; key; string_of_int index] 
  and k = Expect_bulk callback in
  pipe_send_multi connection cmd k

(* LSET *)
let lset connection key index value =
  let cmd = ["LSET"; key; string_of_int index; value] in
  expect_success (send_multi connection cmd)

let plset connection key index value =
  let cmd = ["LSET"; key; string_of_int index; value] 
  and k = Expect_success in
  pipe_send_multi connection cmd k

(* LREM *)
let lrem connection key count value =
  let cmd = ["LREM"; key; string_of_int count; value] in
  expect_int (send_multi connection cmd)

let plrem connection key count value callback =
  let cmd = ["LREM"; key; string_of_int count; value]
  and k = Expect_int callback in
  pipe_send_multi connection cmd k

(* LPOP *)
let lpop connection key =
  expect_bulk (send_multi connection ["LPOP"; key])

let plpop connection key callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["LPOP"; key] k

(* RPOP *)
let rpop connection key =
  expect_bulk (send_multi connection ["RPOP"; key])

let prpop connection key callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["RPOP"; key] k

(* RPOPLPUSH *)
let rpoplpush connection src_key dest_key =
  expect_bulk (send_multi connection ["RPOPLPUSH"; src_key; dest_key])

let prpoplpush connection src_key dest_key callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["RPOPLPUSH"; src_key; dest_key] k

let string_of_timeout = function
  | Seconds seconds -> string_of_int seconds
  | Wait       -> "0"
    
(* BLPOP, but for only one key *)
let blpop connection ?(timeout = Wait) key =
  let cmd = ["BLPOP"; key; string_of_timeout timeout] in
  expect_kv_multi (send_multi connection cmd)

let pblpop connection ?(timeout = Wait) key callback =
  let cmd = ["BLPOP"; key; string_of_timeout timeout]
  and k = Expect_kv_multi callback in
  pipe_send_multi connection cmd k

(* BLPOP, but for many keys *)
let blpop_many connection ?(timeout = Wait) key_list =
  let cmd = ("BLPOP" :: key_list) @ [string_of_timeout timeout] in
  expect_kv_multi (send_multi connection cmd)
    
let pblpop_many connection ?(timeout = Wait) key_list callback =
  let cmd = ("BLPOP" :: key_list) @ [string_of_timeout timeout]
  and k = Expect_kv_multi callback in
  pipe_send_multi connection cmd k
    
(* BRPOP, but for only one key *)
let brpop connection ?(timeout = Wait) key =
  let cmd = ["BRPOP"; key; string_of_timeout timeout] in
  expect_kv_multi (send_multi connection cmd)

let pbrpop connection ?(timeout = Wait) key callback =
  let cmd = ["BRPOP"; key; string_of_timeout timeout] 
  and k = Expect_kv_multi callback in
  pipe_send_multi connection cmd k

(* BRPOP *)
let brpop_many connection ?(timeout = Wait) key_list =
  let cmd = "BRPOP" :: key_list @ [string_of_timeout timeout] in
  expect_kv_multi (send_multi connection cmd)

let pbrpop_many connection ?(timeout = Wait) key_list callback =
  let cmd = "BRPOP" :: key_list @ [string_of_timeout timeout] 
  and k = Expect_kv_multi callback in
  pipe_send_multi connection cmd k

(******************************)
(* Commands operating on sets *)
(******************************)

(* SADD *)
let sadd connection key member =
  expect_bool (send_multi connection ["SADD"; key; member])

let psadd connection key member callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["SADD"; key; member] k

(* SREM *)
let srem connection key member =
  expect_bool (send_multi connection ["SREM"; key; member])

let psrem connection key member callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["SREM"; key; member] k

(* SPOP *)
let spop connection key =
  expect_bulk (send_multi connection ["SPOP"; key])

let pspop connection key callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["SPOP"; key] k

(* SMOVE *)
let smove connection srckey destkey member =
  expect_bool (send_multi connection ["SMOVE"; srckey; destkey; member])

let psmove connection srckey destkey member callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["SMOVE"; srckey; destkey; member] k

(* SCARD *)
let scard connection key =
  expect_int (send_multi connection ["SCARD"; key])

let pscard connection key callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["SCARD"; key] k

(* SISMEMBER *)
let sismember connection key member =
  expect_bool (send_multi connection ["SISMEMBER"; key; member])

let psismember connection key member callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["SISMEMBER"; key; member] k

(* SMEMBERS *)
let smembers connection key =
  expect_list (send_multi connection ["SMEMBERS"; key])

let psmembers connection key callback =
  let k = Expect_list callback in
  pipe_send_multi connection ["SMEMBERS"; key] k

(* SINTER *)
let sinter connection keys =
  expect_list (send_multi connection ("SINTER" :: keys))

let psinter connection keys callback =
  let k = Expect_list callback in
  pipe_send_multi connection ("SINTER" :: keys) k

(* SINTERSTORE *)
let sinterstore connection dstkey keys =
  expect_int (send_multi connection ("SINTERSTORE" :: dstkey :: keys))

let psinterstore connection dstkey keys callback =
  let k = Expect_int callback in
  pipe_send_multi connection ("SINTERSTORE" :: dstkey :: keys) k

(* SUNION *)
let sunion connection keys =
  expect_list (send_multi connection ("SUNION" :: keys))

let psunion connection keys callback =
  let k = Expect_list callback in
  pipe_send_multi connection ("SUNION" :: keys) k

(* SUNIONSTORE *)
let sunionstore connection dstkey keys =
  expect_int (send_multi connection ("SUNIONSTORE" :: dstkey :: keys))

let psunionstore connection dstkey keys callback =
  let k = Expect_int callback in
  pipe_send_multi connection ("SUNIONSTORE" :: dstkey :: keys) k

(* SDIFF *)
let sdiff connection from_key keys =
  expect_list (send_multi connection ("SDIFF" :: from_key :: keys))

let psdiff connection from_key keys callback =
  let k = Expect_list callback in
  pipe_send_multi connection ("SDIFF" :: from_key :: keys) k

(* SDIFFSTORE *)
let sdiffstore connection dstkey from_key keys =
  let cmd = "SDIFFSTORE" :: dstkey :: from_key :: keys in
  expect_int (send_multi connection cmd)

let psdiffstore connection dstkey from_key keys callback =
  let cmd = "SDIFFSTORE" :: dstkey :: from_key :: keys 
  and k = Expect_int callback in
  pipe_send_multi connection cmd k

(* SRANDMEMBER *)
let srandmember connection key =
  expect_bulk (send_multi connection ["SRANDMEMBER"; key])

let psrandmember connection key callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["SRANDMEMBER"; key] k

(****************************************)
(* Multiple databases handling commands *)
(****************************************)

(* SELECT *)
let select connection index =
  expect_success (send_multi connection ["SELECT"; string_of_int index])

let pselect connection index =
  let k = Expect_success in
  pipe_send_multi connection ["SELECT"; string_of_int index] k

(* MOVE *)
let move connection key index =
  expect_bool (send_multi connection ["MOVE"; key; string_of_int index])

let pmove connection key index callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["MOVE"; key; string_of_int index] k

(* FLUSHDB *)
let flushdb connection =
  expect_success (send_multi connection ["FLUSHDB"])

let pflushdb connection =
  let k = Expect_success in
  pipe_send_multi connection ["FLUSHDB"] k

(* FLUSHALL *)
let flushall connection =
  expect_success (send_multi connection ["FLUSHALL"])

let pflushall connection =
  let k = Expect_success in
  pipe_send_multi connection ["FLUSHALL"] k

(*************************************)
(* Commands operating on sorted sets *)
(*************************************)

let format_float = Printf.sprintf "%.8f"

(* ZADD *)
let zadd connection key score member =
  let cmd = ["ZADD"; key; format_float score; member] in
  expect_bool (send_multi connection cmd)

let pzadd connection key score member callback =
  let cmd = ["ZADD"; key; format_float score; member] 
  and k = Expect_bool callback in
  pipe_send_multi connection cmd k

(* ZREM *)
let zrem connection key member =
  expect_bool (send_multi connection ["ZREM"; key; member])

let pzrem connection key member callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["ZREM"; key; member] k

(* ZRANGE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)
let zrange connection key start stop =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop] in
  expect_list (send_multi connection cmd)

let pzrange connection key start stop callback =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop] 
  and k = Expect_list callback in
  pipe_send_multi connection cmd k

(* ZRANGE, but with the WITHSCORES option added on. *)
let zrange_with_scores connection key start stop =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop; "WITHSCORES"] in
  score_transformer (expect_list (send_multi connection cmd))

let pzrange_with_scores connection key start stop callback =
  let cmd = ["ZRANGE"; key; string_of_int start; string_of_int stop; "WITHSCORES"] 
  and k = Score_transformer callback in
  pipe_send_multi connection cmd k

(* ZREVRANGE, please note that the word 'end' is a keyword in ocaml, 
so it has been replaced by 'stop' *)
let zrevrange connection key start stop =
  let cmd = ["ZREVRANGE"; key; string_of_int start; string_of_int stop] in
  expect_list (send_multi connection cmd)

let pzrevrange connection key start stop callback =
  let cmd = ["ZREVRANGE"; key; string_of_int start; string_of_int stop]
  and k = Expect_list callback in
  pipe_send_multi connection cmd k

(* ZRANGE, but with the WITHSCORES option added on. *)
let zrevrange_with_scores connection key start stop =
  let cmd = ["ZREVRANGE"; key; string_of_int start; string_of_int stop; "WITHSCORES"] in
  score_transformer (expect_list (send_multi connection cmd))

let pzrevrange_with_scores connection key start stop callback =
  let cmd = ["ZREVRANGE"; key; string_of_int start; string_of_int stop; "WITHSCORES"]
  and k = Score_transformer callback in
  pipe_send_multi connection cmd k

(* ZRANGEBYSCORE, please note that the word 'end' is a keyword in ocaml, 
   so it has been replaced by 'stop' *)

let zrangebyscore connection ?(limit = Unlimited) key start stop =
  let limit = match limit with
    | Unlimited    -> []
    | Limit (x, y) -> ["LIMIT"; string_of_int x; string_of_int y]
  in
  let cmd = ["ZRANGEBYSCORE"; key; format_float start; format_float stop] @ limit in
  expect_list (send_multi connection cmd)

let pzrangebyscore connection ?(limit = Unlimited) key start stop callback =
  let limit = match limit with
    | Unlimited    -> []
    | Limit (x, y) -> ["LIMIT"; string_of_int x; string_of_int y]
  in
  let cmd = ["ZRANGEBYSCORE"; key; format_float start; format_float stop] @ limit
  and k = Expect_list callback in
  pipe_send_multi connection cmd k

(* ZINCRBY *)
let zincrby connection key increment member =
  let cmd = ["ZINCRBY"; key; format_float increment; member] in
  expect_float (send_multi connection cmd)

let pzincrby connection key increment member callback =
  let cmd = ["ZINCRBY"; key; format_float increment; member] 
  and k = Expect_float callback in
  pipe_send_multi connection cmd k

(* ZRANK *)
let zrank connection key member =
  expect_rank (send_multi connection ["ZRANK"; key; member])

let pzrank connection key member callback =
  let k = Expect_rank callback in
  pipe_send_multi connection ["ZRANK"; key; member] k

(* ZREVRANK *)
let zrevrank connection key member =
  expect_rank (send_multi connection ["ZREVRANK"; key; member])

let pzrevrank connection key member callback =
  let k = Expect_rank callback in
  pipe_send_multi connection ["ZREVRANK"; key; member] k

(* ZCARD *)
let zcard connection key =
  expect_int (send_multi connection ["ZCARD"; key])

let pzcard connection key callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["ZCARD"; key] k

(* ZSCORE *)
let zscore connection key member =
  expect_opt_float (send_multi connection ["ZSCORE"; key; member])

let pzscore connection key member callback =
  let k = Expect_opt_float callback in
  pipe_send_multi connection ["ZSCORE"; key; member] k

(* ZREMRANGEBYRANK *)
let zremrangebyrank connection key start stop =
  let cmd = ["ZREMRANGEBYRANK"; key; string_of_int start; string_of_int stop] in
  expect_int (send_multi connection cmd)

let pzremrangebyrank connection key start stop callback =
  let cmd = ["ZREMRANGEBYRANK"; key; string_of_int start; string_of_int stop]
  and k = Expect_int callback in
  pipe_send_multi connection cmd k

(* ZREMRANGEBYSCORE *)
let zremrangebyscore connection key min max =
  let cmd = ["ZREMRANGEBYSCORE"; key; format_float min; format_float max] in
  expect_int (send_multi connection cmd)

let pzremrangebyscore connection key min max callback =
  let cmd = ["ZREMRANGEBYSCORE"; key; format_float min; format_float max] 
  and k = Expect_int callback in
  pipe_send_multi connection cmd k

let keylen l = string_of_int (List.length l)

let string_of_aggregate a = 
  let s = match a with 
    | Sum -> "SUM" 
    | Min -> "MIN" 
    | Max -> "MAX"
  in 
  ["AGGREGATE"; s] 

let zunioncmd cmd connection ?(aggregate = Sum) dstkey key_list =
  let cmd = [cmd; dstkey; keylen key_list] @ key_list @ (string_of_aggregate aggregate) in
  expect_int (send_multi connection cmd)

let zunionstore connection = zunioncmd "ZUNIONSTORE" connection
let zinterstore connection = zunioncmd "ZINTERSTORE" connection

let pzunioncmd cmd connection ?(aggregate = Sum) dstkey key_list callback =
  let cmd = [cmd; dstkey; keylen key_list] @ key_list @ (string_of_aggregate aggregate)
  and k = Expect_int callback in 
  pipe_send_multi connection cmd k

let pzunionstore connection = pzunioncmd "ZUNIONSTORE" connection
let pzinterstore connection = pzunioncmd "ZINTERSTORE" connection

let zunioncmd_with_weights cmd connection ?(aggregate = Sum) dstkey key_list weight_list =
  if List.length key_list != List.length weight_list
  then failwith ("Not as many weights were given as keys to " ^ cmd);
  let weights = List.map format_float weight_list in
  let cmd = [cmd; dstkey; keylen key_list] 
    @ key_list @ ("WEIGHTS" :: weights) @ (string_of_aggregate aggregate) in
  expect_int (send_multi connection cmd)

let zunionstore_with_weights connection = zunioncmd_with_weights "ZUNIONSTORE" connection
let zinterstore_with_weights connection = zunioncmd_with_weights "ZINTERSTORE" connection

let pzunioncmd_with_weights cmd connection ?(aggregate = Sum) dstkey key_list weight_list callback =
  if List.length key_list != List.length weight_list
  then failwith ("Not as many weights were given as keys to " ^ cmd);
  let weights = List.map format_float weight_list in
  let cmd = [cmd; dstkey; keylen key_list] @ key_list @ ("WEIGHTS" :: weights) @ (string_of_aggregate aggregate)
  and k = Expect_int callback in
  pipe_send_multi connection cmd k

let pzunionstore_with_weights connection = pzunioncmd_with_weights "ZUNIONSTORE" connection
let pzinterstore_with_weights connection = pzunioncmd_with_weights "ZINTERSTORE" connection

(********************************)
(* Commands operating on hashes *)
(********************************)

(* HSET *)
let hset connection key field value =
  expect_bool (send_multi connection ["HSET"; key; field; value])

let phset connection key field value callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["HSET"; key; field; value] k

(* HSETNX *)
let hsetnx connection key field value =
  expect_bool (send_multi connection ["HSETNX"; key; field; value])

let phsetnx connection key field value callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["HSETNX"; key; field; value] k

(* HDEL *)
let hdel connection key field =
  expect_bool (send_multi connection ["HDEL"; key; field])
    
let phdel connection key field callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["HDEL"; key; field] k
    
(* HGET *)
let hget connection key field =
  expect_bulk (send_multi connection ["HGET"; key; field])

let phget connection key field callback =
  let k = Expect_bulk callback in
  pipe_send_multi connection ["HGET"; key; field] k

(* HMGET *)
let hmget connection key field_list =
  let cmd = "HMGET" :: key :: field_list in
  expect_multi (send_multi connection cmd)

let phmget connection key field_list callback =
  let cmd = "HMGET" :: key :: field_list 
  and k = Expect_multi callback in
  pipe_send_multi connection cmd k

(* HMSET *)
let hmset connection key field_value_pairs =
  let f rest el = (fst el) :: (snd el) :: rest in
  let values = List.fold_left f [] field_value_pairs in
  expect_success (send_multi connection ("HMSET" :: key :: values))

let phmset connection key field_value_pairs =
  let f rest el = (fst el) :: (snd el) :: rest in
  let values = List.fold_left f [] field_value_pairs in
  let k = Expect_success in
  pipe_send_multi connection ("HMSET" :: key :: values) k

(* HINCRBY *)
let hincrby connection key field value =
  let cmd = ["HINCRBY"; key; field; string_of_int value] in
  expect_large_int (send_multi connection cmd)

let phincrby connection key field value callback =
  let cmd = ["HINCRBY"; key; field; string_of_int value] in
  let k = Expect_large_int callback in
  pipe_send_multi connection cmd k

(* HEXISTS *)
let hexists connection key field =
  expect_bool (send_multi connection ["HEXISTS"; key; field])

let phexists connection key field callback =
  let k = Expect_bool callback in
  pipe_send_multi connection ["HEXISTS"; key; field] k

(* HLEN *)
let hlen connection key =
  expect_int (send_multi connection ["HLEN"; key])

let phlen connection key callback =
  let k = Expect_int callback in
  pipe_send_multi connection ["HLEN"; key] k

(* HKEYS *)
let hkeys connection key =
  expect_list (send_multi connection ["HKEYS"; key])

let phkeys connection key callback =
  let k = Expect_list callback in
  pipe_send_multi connection ["HKEYS"; key] k

(* HVALS *)
let hvals connection key =
  expect_list (send_multi connection ["HVALS"; key])

let phvals connection key callback =
  let k = Expect_list callback in
  pipe_send_multi connection ["HVALS"; key] k

(* HGETALL *)
let hgetall connection key =
  let result = expect_list (send_multi connection ["HGETALL"; key]) in
  collate (fun x -> x) result

let phgetall connection key callback =
  let k = Collate callback in
  pipe_send_multi connection ["HGETALL"; key] k

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
  expect_list (send connection command)

let psort connection
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(get = NoPattern)
    ?(order = Asc)
    ?(alpha = NonAlpha) 
    key callback =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    "SORT " ^ key ^ pattern ^ limit ^ (parse_get_arg get) ^ order ^ alpha 
  and k = Expect_list callback in
  pipe_send connection command k

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
  let patlen = List.length get_patterns in
  let reply = send connection command in
  collate_n patlen (expect_list reply)

let psort_get_many connection 
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(order = Asc)
    ?(alpha = NonAlpha) 
    key get_patterns callback =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    let f rest n = rest ^ " GET " ^ n in
    let get = List.fold_left f "" get_patterns in
    "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha in
  let patlen = List.length get_patterns in
  let k = Collate_n (patlen, callback) in
  pipe_send connection command k

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

let psort_and_store connection
    ?(pattern = NoPattern)
    ?(limit = Unlimited)
    ?(order = Asc)
    ?(alpha = NonAlpha) 
    key get_patterns dest_key callback =
  let command = 
    let pattern, limit, order, alpha =
      parse_sort_args pattern limit order alpha in
    let f rest n = rest ^ " GET " ^ n in
    let get = List.fold_left f "" get_patterns in
    let store = " STORE " ^ dest_key in
    "SORT " ^ key ^ pattern ^ limit ^ get ^ order ^ alpha ^ store in
  let k = Expect_int callback in
  pipe_send connection command k

(********************************)
(* Persistence control commands *)
(********************************)

(* SAVE *)
let save connection =
  expect_success (send connection "SAVE")

let psave connection =
  let k = Expect_success in
  pipe_send connection "SAVE" k

(* BGSAVE *)
let bgsave connection =
  expect_status "Background saving started" (send connection "BGSAVE")

let pbgsave connection =
  let k = Expect_status "Background saving started" in
  pipe_send connection "BGSAVE" k

(* LASTSAVE *)
let lastsave connection =
  expect_large_int (send connection "LASTSAVE")

let plastsave connection callback =
  let k = Expect_large_int callback in
  pipe_send connection "LASTSAVE" k

(* SHUTDOWN *)
let shutdown connection =
  just_send connection "SHUTDOWN";
  try
    match recv connection with
      | Status x -> failwith x 
      | _        -> failwith "Did not recognize what I got back"
  with End_of_file -> ()

(* BGREWRITEAOF *)
let bgrewriteaof connection =
  expect_status 
    "Background append only file rewriting started"
    (send connection "BGREWRITEAOF")

let pbgrewriteaof connection =
  let k = Expect_status "Background append only file rewriting started" in
  pipe_send connection "BGREWRITEAOF" k

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

  let create = function
    | None -> 
      failwith "No server info"
    | Some text -> 
      let values = Hashtbl.create 10 in
      let loader (key, value) = Hashtbl.add values key value; key in
      let fields = List.map loader (tokenizer text) in
      {fields; values}

  let get info field = Hashtbl.find info.values field
  let get_fields info = info.fields

end

(* INFO *)
let info connection =
  Info.create (expect_bulk (send connection "INFO"))

(* SLAVEOF *)
let slaveof connection addr port =
  expect_success (send connection (Printf.sprintf "SLAVEOF %s %d" addr port))

