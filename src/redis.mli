(**
Redis is a module used to interact with a Redis key value store. For a full description of all the redis keywords, refer to the redis docs linked below.

@see <http://code.google.com/p/redis/> the Redis project.
*)

(** {3:redis_types Types used with redis} *)

(** Different types of redis keys, as per the TYPE keyword. To get a string representation, use {!Redis.string_of_redis_value_type}. *)
type redis_value_type = RedisString | RedisNil | RedisList | RedisSet

(** Bulk types. To get a string representation, use {!Redis.string_of_bulk_data}. *)
type bulk_data = Nil | String of string

(** All redis response types. To get a string representation, use {!Redis.string_of_response}. *)
type response = Status of string
  | Undecipherable
  | Integer of int
  | LargeInteger of float
  | Bulk of bulk_data
  | Multibulk of bulk_data list
  | Error of string

(** Gives a string representation of a {!Redis.redis_value_type} type. *)
val string_of_redis_value_type : redis_value_type -> string

(** Gives a string representation of a {!Redis.bulk_data} type. *)
val string_of_bulk_data : bulk_data -> string

(** Gives a string representation of a {!Redis.response} type. *)
val string_of_response : response -> string

(** {3:connection Connection handling} *)

(** The [Connection] module is used to abstract away all the socket manipulations done by the {!Redis} module. *)
module Connection :
    sig
        (** This type is used to represent the connection to be passed around to all the functions. *)
        type t = in_channel * out_channel
    end

(** Returns a {!Connection.t} to be used by all the {!Redis} functions.
    @param addr Address of the Redis server. Defaults to the localhost ([127.0.0.1]).
    @param port Port of the Redis server. Defaults to the default redis port ([6379]).
*)
val create_connection : ?addr:string -> ?port:int -> unit -> Connection.t


(** Ping the server to see if it is up, as per the [PING] redis keyword. *)
val ping : Connection.t -> bool

(** Close connection to redis, as per the [QUIT] redis keyword. *)
val quit : Connection.t -> unit

(** [auth p c] authenticates connection [c] with password [p], as per the [AUTH] redis keyword. *)
val auth : string -> Connection.t -> unit

(** {3:all_cmds Commands operating on all the kind of values} *)

(** [exists k c] checks if key [k] exists on connection [c], as per the [EXISTS] redis keyword. *)
val exists : string -> Connection.t -> bool

(** [del kl c] deletes all the keys in the list [kl] on connection [c], as per the [DEL] redis keyword.
    @return the number of deleted keys.
*)
val del : string list -> Connection.t -> int

(** [del_one k c] deletes the key [k] on connection [c]. This is a utility function to avoid creating a list of one key with the {!Redis.del} function.
    @return [true] if the key was deleted.
*)
val del_one : string -> Connection.t -> bool

(** [value_type k c] returns the type of key [k] on connection [c], as per the [TYPE] redis keyword. Note that this is not named type because [type] is a reserved keyword in Ocaml. *)
val value_type :
  string -> Connection.t -> redis_value_type

(** [keys p c] returns a list of keys matching the pattern [p] on connection [c], as per the [KEYS] redis keyword. *)
val keys : string -> Connection.t -> string list

(** [randomkey c] returns a random key on connection [c], as per the [RANDOMKEY] redis keyword. *)
val randomkey : Connection.t -> string

(** [rename k nk c] renames key [k] to new key [nk] on connection [c], as per the [RENAME] redis keyword. *)
val rename : string -> string -> Connection.t -> unit

(** [renamenx k nk c] renames key [k] to new key [nk] on connection [c], much like {!rename}, except it will return [true] if the new key does not already exists, [false] otherwise, as per the [RENAMENX] redis keyword. *)
val renamenx : string -> string -> Connection.t -> bool

(** [dbsize c] returns the number of keys in the database *)
val dbsize : Connection.t -> int

(** [expire k s c] sets the expire time of key [k] to [s] seconds on connection [c], as per the [EXPIRE] redis keyword.
    @return [true] if the timeout was set, false if the key does not exist or already has an associated timeout.
*)
val expire : string -> int -> Connection.t -> bool

(** [ttl k c] returns the time to live in seconds for key [k] on connection [c], as per the [TTL] redis keyword. *)
val ttl : string -> Connection.t -> int

(** [select db c] selects database n. [db] on connection [c], as per the [SELECT] redis keyword. *)
val select : int -> Connection.t -> unit

(** [move k db c] moves key [k] to database n. [db] on connection [c], as per the [MOVE] redis keyword.
    @return [false] if the key [k] exists in the database [db] or does not exist in the current database.
*)
val move : string -> int -> Connection.t -> bool

(** [flushdb c] flushes all the keys from the currently selected database, as per the [FLUSHDB] redis keyword. *)
val flushdb : Connection.t -> unit

(** [flushall c] flushes all the keys from all the databases, as per the [FLUSHALL] redis keyword. *)
val flushall : Connection.t -> unit

(** {3:string_cmd Commands operating on string values} *)

(** [set k v c] sets key [k] to value [v] on connection [c], as per the [SET] redis keyword. *)
val set : string -> string -> Connection.t -> unit

(** [get k c] gets the key [k] on connection [c], as per the [GET] redis keyword. *)
val get : string -> Connection.t -> bulk_data

(** [getset k v c] gets the key [k] on connection [c], and then sets it to [v], as per the [GETSET] redis keyword. *)
val getset :
  string -> string -> Connection.t -> bulk_data

(** [mget k c] gets the list of keys [k] on connection [c], as per the [MGET] redis keyword. *)
val mget :
  string list -> Connection.t -> bulk_data list

(** [getset k v] gets the ket [k] on connection [c], and then sets it to [v], as per the [GETSET] redis keyword. *)
val setnx : string -> string -> Connection.t -> bool
val mset : (string * string) list -> Connection.t -> unit
val msetnx : (string * string) list -> Connection.t -> bool
val incr : string -> Connection.t -> int
val incrby : string -> int -> Connection.t -> int
val decr : string -> Connection.t -> int
val decrby : string -> int -> Connection.t -> int

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
