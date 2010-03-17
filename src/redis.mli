val create_connection : ?addr:string -> ?port:int -> unit -> Redis_util.Connection.t
val ping : Redis_util.Connection.t -> bool
val quit : Redis_util.Connection.t -> unit
val auth : string -> Redis_util.Connection.t -> unit
val set : string -> string -> Redis_util.Connection.t -> unit
val get : string -> Redis_util.Connection.t -> Redis_util.bulk_data
val getset :
  string -> string -> Redis_util.Connection.t -> Redis_util.bulk_data
val mget :
  string list -> Redis_util.Connection.t -> Redis_util.bulk_data list
val setnx : string -> string -> Redis_util.Connection.t -> bool
val mset : (string * string) list -> Redis_util.Connection.t -> unit
val msetnx : (string * string) list -> Redis_util.Connection.t -> bool
val incr : string -> Redis_util.Connection.t -> int
val incrby : string -> int -> Redis_util.Connection.t -> int
val decr : string -> Redis_util.Connection.t -> int
val decrby : string -> int -> Redis_util.Connection.t -> int
val exists : string -> Redis_util.Connection.t -> bool
val del : string list -> Redis_util.Connection.t -> int
val del_one : string -> Redis_util.Connection.t -> bool
val value_type :
  string -> Redis_util.Connection.t -> Redis_util.redis_value_type
val keys : string -> Redis_util.Connection.t -> string list
val randomkey : Redis_util.Connection.t -> string
val rename : string -> string -> Redis_util.Connection.t -> unit
val renamenx : string -> string -> Redis_util.Connection.t -> bool
val dbsize : Redis_util.Connection.t -> int
val expire : string -> int -> Redis_util.Connection.t -> bool
val ttl : string -> Redis_util.Connection.t -> int
val rpush : string -> string -> Redis_util.Connection.t -> unit
val lpush : string -> string -> Redis_util.Connection.t -> unit
val llen : string -> Redis_util.Connection.t -> int
val lrange :
  string ->
  int -> int -> Redis_util.Connection.t -> Redis_util.bulk_data list
val ltrim : string -> int -> int -> Redis_util.Connection.t -> unit
val lindex :
  string -> int -> Redis_util.Connection.t -> Redis_util.bulk_data
val lset : string -> int -> string -> Redis_util.Connection.t -> unit
val lrem : string -> int -> string -> Redis_util.Connection.t -> int
val lpop : string -> Redis_util.Connection.t -> Redis_util.bulk_data
val rpop : string -> Redis_util.Connection.t -> Redis_util.bulk_data
val rpoplpush : string -> string -> Redis_util.Connection.t -> Redis_util.bulk_data
val sadd : string -> string -> Redis_util.Connection.t -> bool
val srem : string -> string -> Redis_util.Connection.t -> bool
val spop : string -> Redis_util.Connection.t -> Redis_util.bulk_data
val smove : string -> string -> string -> Redis_util.Connection.t -> bool
val scard : string -> Redis_util.Connection.t -> int
val sismember : string -> string -> Redis_util.Connection.t -> bool
val smembers :
  string -> Redis_util.Connection.t -> Redis_util.bulk_data list
val sinter :
  string list -> Redis_util.Connection.t -> Redis_util.bulk_data list
val sinterstore : string -> string list -> Redis_util.Connection.t -> int
val sunion :
  string list -> Redis_util.Connection.t -> Redis_util.bulk_data list
val sunionstore : string -> string list -> Redis_util.Connection.t -> int
val sdiff :
  string list -> Redis_util.Connection.t -> Redis_util.bulk_data list
val sdiffstore : string -> string list -> Redis_util.Connection.t -> int
val srandmember : string -> Redis_util.Connection.t -> Redis_util.bulk_data
val select : int -> Redis_util.Connection.t -> unit
val move : string -> int -> Redis_util.Connection.t -> bool
val flushdb : Redis_util.Connection.t -> unit
val flushall : Redis_util.Connection.t -> unit
val zadd : string -> float -> string -> Redis_util.Connection.t -> bool
val zrem : string -> string -> Redis_util.Connection.t -> bool
val zrange :
  string ->
  int -> int -> Redis_util.Connection.t -> Redis_util.bulk_data list
val zrevrange :
  string ->
  int -> int -> Redis_util.Connection.t -> Redis_util.bulk_data list
val zrangebyscore :
  string ->
  float ->
  float ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  Redis_util.Connection.t -> Redis_util.bulk_data list
val zincrby : string -> float -> string -> Redis_util.Connection.t -> float
val zcard : string -> Redis_util.Connection.t -> int
val zscore : string -> string -> Redis_util.Connection.t -> float
val zremrangebyscore : string -> float -> float -> Redis_util.Connection.t -> int
val sort :
  string ->
  ?pattern:string ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  ?get:string ->
  ?order:[< `Asc | `Desc > `Asc ] ->
  ?alpha:[< `Alpha | `NonAlpha > `NonAlpha ] ->
  Redis_util.Connection.t -> Redis_util.bulk_data list
val save : Redis_util.Connection.t -> unit
val bgsave : Redis_util.Connection.t -> unit
val bgrewriteaof : Redis_util.Connection.t -> unit
val lastsave : Redis_util.Connection.t -> float
val shutdown : Redis_util.Connection.t -> unit

module Info :
    sig
        type t = {fields: string list; values: (string, string) Hashtbl.t;}
        val create : string -> t
        val get : t -> string -> string
        val get_fields : t -> string list
    end
val info : Redis_util.Connection.t -> Info.t
