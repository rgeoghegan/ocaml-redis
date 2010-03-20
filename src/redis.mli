type redis_value_type = RedisString | RedisNil | RedisList | RedisSet
type bulk_data = Nil | String of string
type response = Status of string
  | Undecipherable
  | Integer of int
  | LargeInteger of float
  | Bulk of bulk_data
  | Multibulk of bulk_data list
  | Error of string

val string_of_redis_value_type : redis_value_type -> string
val string_of_bulk_data : bulk_data -> string
val string_of_response : response -> string

module Connection :
    sig
        type t = in_channel * out_channel
    end

val create_connection : ?addr:string -> ?port:int -> unit -> Connection.t
val ping : Connection.t -> bool
val quit : Connection.t -> unit
val auth : string -> Connection.t -> unit
val set : string -> string -> Connection.t -> unit
val get : string -> Connection.t -> bulk_data
val getset :
  string -> string -> Connection.t -> bulk_data
val mget :
  string list -> Connection.t -> bulk_data list
val setnx : string -> string -> Connection.t -> bool
val mset : (string * string) list -> Connection.t -> unit
val msetnx : (string * string) list -> Connection.t -> bool
val incr : string -> Connection.t -> int
val incrby : string -> int -> Connection.t -> int
val decr : string -> Connection.t -> int
val decrby : string -> int -> Connection.t -> int
val exists : string -> Connection.t -> bool
val del : string list -> Connection.t -> int
val del_one : string -> Connection.t -> bool
val value_type :
  string -> Connection.t -> redis_value_type
val keys : string -> Connection.t -> string list
val randomkey : Connection.t -> string
val rename : string -> string -> Connection.t -> unit
val renamenx : string -> string -> Connection.t -> bool
val dbsize : Connection.t -> int
val expire : string -> int -> Connection.t -> bool
val ttl : string -> Connection.t -> int
val rpush : string -> string -> Connection.t -> unit
val lpush : string -> string -> Connection.t -> unit
val llen : string -> Connection.t -> int
val lrange :
  string ->
  int -> int -> Connection.t -> bulk_data list
val ltrim : string -> int -> int -> Connection.t -> unit
val lindex :
  string -> int -> Connection.t -> bulk_data
val lset : string -> int -> string -> Connection.t -> unit
val lrem : string -> int -> string -> Connection.t -> int
val lpop : string -> Connection.t -> bulk_data
val rpop : string -> Connection.t -> bulk_data
val rpoplpush : string -> string -> Connection.t -> bulk_data
val sadd : string -> string -> Connection.t -> bool
val srem : string -> string -> Connection.t -> bool
val spop : string -> Connection.t -> bulk_data
val smove : string -> string -> string -> Connection.t -> bool
val scard : string -> Connection.t -> int
val sismember : string -> string -> Connection.t -> bool
val smembers :
  string -> Connection.t -> bulk_data list
val sinter :
  string list -> Connection.t -> bulk_data list
val sinterstore : string -> string list -> Connection.t -> int
val sunion :
  string list -> Connection.t -> bulk_data list
val sunionstore : string -> string list -> Connection.t -> int
val sdiff :
  string list -> Connection.t -> bulk_data list
val sdiffstore : string -> string list -> Connection.t -> int
val srandmember : string -> Connection.t -> bulk_data
val select : int -> Connection.t -> unit
val move : string -> int -> Connection.t -> bool
val flushdb : Connection.t -> unit
val flushall : Connection.t -> unit
val zadd : string -> float -> string -> Connection.t -> bool
val zrem : string -> string -> Connection.t -> bool
val zrange :
  string ->
  int -> int -> Connection.t -> bulk_data list
val zrevrange :
  string ->
  int -> int -> Connection.t -> bulk_data list
val zrangebyscore :
  string ->
  float ->
  float ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  Connection.t -> bulk_data list
val zincrby : string -> float -> string -> Connection.t -> float
val zcard : string -> Connection.t -> int
val zscore : string -> string -> Connection.t -> float
val zremrangebyscore : string -> float -> float -> Connection.t -> int
val sort :
  string ->
  ?pattern:string ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  ?get:string ->
  ?order:[< `Asc | `Desc > `Asc ] ->
  ?alpha:[< `Alpha | `NonAlpha > `NonAlpha ] ->
  Connection.t -> bulk_data list
val save : Connection.t -> unit
val bgsave : Connection.t -> unit
val bgrewriteaof : Connection.t -> unit
val lastsave : Connection.t -> float
val shutdown : Connection.t -> unit

module Info :
    sig
        type t = {fields: string list; values: (string, string) Hashtbl.t;}
        val create : string -> t
        val get : t -> string -> string
        val get_fields : t -> string list
    end
val info : Connection.t -> Info.t
