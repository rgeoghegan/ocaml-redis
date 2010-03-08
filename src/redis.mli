val create_connection : string -> int -> in_channel * out_channel
val ping : in_channel * out_channel -> bool
val quit : 'a * out_channel -> unit
val auth : string -> in_channel * out_channel -> unit
val set : string -> string -> in_channel * out_channel -> unit
val get : string -> in_channel * out_channel -> Redis_util.bulk_data
val getset :
  string -> string -> in_channel * out_channel -> Redis_util.bulk_data
val mget :
  string list -> in_channel * out_channel -> Redis_util.bulk_data list
val setnx : string -> string -> in_channel * out_channel -> bool
val mset : (string * string) list -> in_channel * out_channel -> unit
val msetnx : (string * string) list -> in_channel * out_channel -> bool
val incr : string -> in_channel * out_channel -> int
val incrby : string -> int -> in_channel * out_channel -> int
val decr : string -> in_channel * out_channel -> int
val decrby : string -> int -> in_channel * out_channel -> int
val exists : string -> in_channel * out_channel -> bool
val del : string list -> in_channel * out_channel -> int
val del_one : string -> in_channel * out_channel -> bool
val value_type :
  string -> in_channel * out_channel -> Redis_util.redis_value_type
val keys : string -> in_channel * out_channel -> string list
val randomkey : in_channel * out_channel -> string
val rename : string -> string -> in_channel * out_channel -> unit
val renamenx : string -> string -> in_channel * out_channel -> bool
val dbsize : in_channel * out_channel -> int
val expire : string -> int -> in_channel * out_channel -> bool
val ttl : string -> in_channel * out_channel -> int
val rpush : string -> string -> in_channel * out_channel -> unit
val lpush : string -> string -> in_channel * out_channel -> unit
val llen : string -> in_channel * out_channel -> int
val lrange :
  string ->
  int -> int -> in_channel * out_channel -> Redis_util.bulk_data list
val ltrim : string -> int -> int -> in_channel * out_channel -> unit
val lindex :
  string -> int -> in_channel * out_channel -> Redis_util.bulk_data
val lset : string -> int -> string -> in_channel * out_channel -> unit
val lrem : string -> int -> string -> in_channel * out_channel -> int
val lpop : string -> in_channel * out_channel -> Redis_util.bulk_data
val rpop : string -> in_channel * out_channel -> Redis_util.bulk_data
val rpoplpush : string -> string -> in_channel * out_channel -> Redis_util.bulk_data
val sadd : string -> string -> in_channel * out_channel -> bool
val srem : string -> string -> in_channel * out_channel -> bool
val spop : string -> in_channel * out_channel -> Redis_util.bulk_data
val smove : string -> string -> string -> in_channel * out_channel -> bool
val scard : string -> in_channel * out_channel -> int
val sismember : string -> string -> in_channel * out_channel -> bool
val smembers :
  string -> in_channel * out_channel -> Redis_util.bulk_data list
val sinter :
  string list -> in_channel * out_channel -> Redis_util.bulk_data list
val sinterstore : string -> string list -> in_channel * out_channel -> int
val sunion :
  string list -> in_channel * out_channel -> Redis_util.bulk_data list
val sunionstore : string -> string list -> in_channel * out_channel -> int
val sdiff :
  string list -> in_channel * out_channel -> Redis_util.bulk_data list
val sdiffstore : string -> string list -> in_channel * out_channel -> int
val srandmember : string -> in_channel * out_channel -> Redis_util.bulk_data
val select : int -> in_channel * out_channel -> unit
val move : string -> int -> in_channel * out_channel -> bool
val flushdb : in_channel * out_channel -> unit
val flushall : in_channel * out_channel -> unit
val zadd : string -> float -> string -> in_channel * out_channel -> bool
val zrem : string -> string -> in_channel * out_channel -> bool
val zrange :
  string ->
  int -> int -> in_channel * out_channel -> Redis_util.bulk_data list
val zrevrange :
  string ->
  int -> int -> in_channel * out_channel -> Redis_util.bulk_data list
val zrangebyscore :
  string ->
  float ->
  float ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  in_channel * out_channel -> Redis_util.bulk_data list
val zincrby : string -> float -> string -> in_channel * out_channel -> float
val zcard : string -> in_channel * out_channel -> int
val zscore : string -> string -> in_channel * out_channel -> float
val zremrangebyscore : string -> float -> float -> in_channel * out_channel -> int
val sort :
  string ->
  ?pattern:string ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  ?get:string ->
  ?order:[< `Asc | `Desc > `Asc ] ->
  ?alpha:[< `Alpha | `NonAlpha > `NonAlpha ] ->
  in_channel * out_channel -> Redis_util.bulk_data list
val save : in_channel * out_channel -> unit
val bgsave : in_channel * out_channel -> unit
val bgrewriteaof : in_channel * out_channel -> unit
val lastsave : in_channel * out_channel -> Big_int.big_int
val shutdown : in_channel * out_channel -> unit
