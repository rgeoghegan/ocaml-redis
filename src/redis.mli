(**
Redis is a module used to interact with a redis-key value store server. For a full description of all the redis keywords, refer to the redis docs linked below.

@see <http://code.google.com/p/redis/> the redis project.
*)

(** {3:redis_types Types used with redis} *)

(** Different types of redis keys, as per the TYPE keyword. To get a string representation, use {!string_of_redis_value_type}. *)
type redis_value_type = RedisString | RedisNil | RedisList | RedisSet

(** Bulk types. To get a string representation, use {!string_of_bulk_data}. *)
type bulk_data = Nil | String of string

(** All redis response types. To get a string representation, use {!string_of_response}. *)
type response = Status of string
  | Undecipherable
  | Integer of int
  | LargeInteger of float
  | Bulk of bulk_data
  | Multibulk of bulk_data list
  | Error of string

(** Gives a string representation of a {!redis_value_type} type. *)
val string_of_redis_value_type : redis_value_type -> string

(** Gives a string representation of a {!bulk_data} type. *)
val string_of_bulk_data : bulk_data -> string

(** Gives a string representation of a {!response} type. *)
val string_of_response : response -> string

(** {3:connection Connection handling} *)

(** The [Connection] module is used to abstract away all the socket manipulations done by the {!Redis} module. *)
module Connection :
    sig
        (** This type is used to represent the connection to be passed around to all the functions. *)
        type t = in_channel * out_channel
    end

(** Returns a {!Connection.t} to be used by all the {!Redis} functions.
    @param addr Address of the redis server. Defaults to the localhost ([127.0.0.1]).
    @param port Port of the redis server. Defaults to the default redis port ([6379]).
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

(** [del_one k c] deletes the key [k] on connection [c]. This is a utility function to avoid creating a list of one key with the {!del} function.
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

(** [rename on nn c] renames old key name [on] to new key name [nn] on connection [c], as per the [RENAME] redis keyword. *)
val rename : string -> string -> Connection.t -> unit

(** [renamenx on nn c] renames old key name [on] to new key name [nn] on connection [c], much like {!rename}, except it will return [true] if the new key does not already exists, [false] otherwise, as per the [RENAMENX] redis keyword. *)
val renamenx : string -> string -> Connection.t -> bool

(** [dbsize c] returns the number of keys in the database *)
val dbsize : Connection.t -> int

(** [expire k s c] sets the expire time of key [k] to [s] seconds on connection [c], as per the [EXPIRE] redis keyword.
    @return [true] if the timeout was set, false if the key does not exist or already has an associated timeout.
*)
val expire : string -> int -> Connection.t -> bool

(** [ttl k c] returns the time to live in seconds for key [k] on connection [c], as per the [TTL] redis keyword. *)
val ttl : string -> Connection.t -> int

(** [select i c] selects database index [i] on connection [c], as per the [SELECT] redis keyword. *)
val select : int -> Connection.t -> unit

(** [move k db c] moves key [k] to database index [db] on connection [c], as per the [MOVE] redis keyword.
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

(** [mget kl c] gets the values associated to each key in list [kl] on connection [c], as per the [MGET] redis keyword. *)
val mget :
  string list -> Connection.t -> bulk_data list

(** [setnx k v c] sets key [k] to value [v] on connection [c], as per the [SETNX] redis keyword. As opposed to {!set}, will return [false] and not set the key if the key [k] already exists; otherwise returns [true]. *)
val setnx : string -> string -> Connection.t -> bool

(** [mset kv c] sets the list of key-value pairs [kv] on connection [c], as per the [MSET] redis keyword. *)
val mset : (string * string) list -> Connection.t -> unit

(** [msetnx kv c] sets the list of key-value pairs [kv] on connection [c], as per the [MSETNX] redis keyword. As opposed to {!mset}, will return [false] and not set {i any} of the given keys if any of them exist already. *)
val msetnx : (string * string) list -> Connection.t -> bool

(** [incr k c] increments key [k] by 1 on connection [c], as per the [INCR] redis keyword.
    @return the new value of the key.
*)
val incr : string -> Connection.t -> int

(** [incrby k i c] increments key [k] by interger [i] on connection [c], as per the [INCRBY] redis keyword.
    @return the new value of the key.
*)
val incrby : string -> int -> Connection.t -> int

(** [decr k c] decrements key [k] by 1 on connection [c], as per the [DECR] redis keyword.
    @return the new value of the key.
*)
val decr : string -> Connection.t -> int

(** [decrby k i c] decrements key [k] by interger [i] on connection [c], as per the [DECRBY] redis keyword.
    @return the new value of the key.
*)
val decrby : string -> int -> Connection.t -> int

(** {3:list_cmd Commands operating on lists} *)

(** [rpush k v c] pushes value [v] to the tail of the list at key [k] on connection [c], as per the [RPUSH] redis keyword. *)
val rpush : string -> string -> Connection.t -> unit

(** [lpush k v c] pushes value [v] to the head of the list at key [k] on connection [c], as per the [LPUSH] redis keyword. *)
val lpush : string -> string -> Connection.t -> unit

(** [llen k c] returns the length of list as key [k] on connection [c], as per the [LLEN] redis keyword. *)
val llen : string -> Connection.t -> int

(** [lrange k s e c] returns the elements of list as key [k] between start index [s] and end index [e] inclusively on connection [c], as per the [LRANGE] redis keyword. *)
val lrange :
  string ->
  int -> int -> Connection.t -> bulk_data list

(** [ltrim k s e c] remove all elements of the list at key [k] {i not} between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ltrim] redis keyword. *)
val ltrim : string -> int -> int -> Connection.t -> unit

(** [lindex k i c] get the value at index [i] in the list at key [k] on connection [c], as per the [LINDEX] redis keyword. *)
val lindex :
  string -> int -> Connection.t -> bulk_data

(** [lset k i v c] sets the index [i] of the list at key [k] to value [v] on connection [c], as per the [LSET] redis keyword. *)
val lset : string -> int -> string -> Connection.t -> unit

(** [lrem k ct v c] removes up to count [ct] values [v] from list at key [k] on connection [c], as per the [LREM] redis keyword.
    @return the number of values removed.
*)
val lrem : string -> int -> string -> Connection.t -> int

(** [lpop k c] pops the head of the list at key [k] on connection [c], as per the [LPOP] redis keyword.
    @return the value popped.
*)
val lpop : string -> Connection.t -> bulk_data

(** [rpop k c] pops the tail of the list at key [k] on connection [c], as per the [RPOP] redis keyword.
    @return the value popped.
*)
val rpop : string -> Connection.t -> bulk_data

(** [rpoplpush sk dk c] pops the tail of the list at the source key [sk] and pushes it to the tail of the list at the destination key [dk] on connection [c], as per the [RPOPLPUSH] redis keyword. *)
val rpoplpush : string -> string -> Connection.t -> bulk_data

(** {3:set_cmd Commands operating on sets} *)

(** [sadd k m c] add member [m] to set at key [k] on connection [c], as per the [SADD] redis keyword.
    @return [true] if member [m] not part of the set already, [false] otherwise.
*)
val sadd : string -> string -> Connection.t -> bool

(** [srem k m c] removes member [m] from the set at key [k] on connection [c], as per the [SREM] redis keyword.
    @return [true] if member [m] was part of the set, [false] otherwise.
*)
val srem : string -> string -> Connection.t -> bool

(** [spop k c] pop a random member from the set at key [k] on connection [c], as per the [SPOP] redis keyword.
    @return the member that got popped.
*)
val spop : string -> Connection.t -> bulk_data

(** [smove sk dk m c] moves the member [m] from the set at the source key [sk] to the set at the destination key [dk] on connection c, as per the [SMOVE] redis keyword.
    @return [false] if the element was not found in the first set and no operation was done, [false] otherwise. 
*)
val smove : string -> string -> string -> Connection.t -> bool

(** [scard k c] returns the number of members in the set at key [k] on connection [c], as per the [SCARD] redis keyword. *)
val scard : string -> Connection.t -> int

(** [sismember k m c] checks if the member [m] is in the set at key [k] exists on connection [c], as per the [SISMEMBER] redis keyword. *)
val sismember : string -> string -> Connection.t -> bool

(** [sinter kl c] returns the intersection of all the sets at the keys listed in [kl] on connection [c], as per the [SINTER] redis keyword. *)
val sinter :
  string list -> Connection.t -> bulk_data list

(** [sinterstore dk kl c] puts the intersection of all the sets listed in [kl] into the set at destination key [dk] on connection [c], as per the [SINTERSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sinterstore : string -> string list -> Connection.t -> int

(** [sunion kl c] returns the union of all the sets at the keys listed in [kl] on connection [c], as per the [SUNION] redis keyword. *)
val sunion :
  string list -> Connection.t -> bulk_data list

(** [sunionstore dk kl c] puts the union of all the sets listed in [kl] into the set at destination key [dk] on connection [c], as per the [SUNIONSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sunionstore : string -> string list -> Connection.t -> int

(** [sdiff kl c] returns the difference between the set at the first key in [kl] and the other keys in the [kl] on connection [c], as per the [SDIFF] redis keyword. *)
val sdiff :
  string list -> Connection.t -> bulk_data list

(** [sdiffstore dk kl c] puts the difference between the set at the first key in [kl] and the other keys in the [kl] into the set at the destination key [dk] on connection [c], as per the [SDIFFSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sdiffstore : string -> string list -> Connection.t -> int

(** [smembers k c] returns all the members of the set at key [k] on connection [c], as per the [SMEMBERS] redis keyword. *)
val smembers :
  string -> Connection.t -> bulk_data list

(** [srandmember k c] returns a random member from the set at the key [k] on connection [c], as per the [SRANDMEMBER] redis keyword. *)
val srandmember : string -> Connection.t -> bulk_data

(** {3:sorted_sets_cmd Commands operating on sorted sets (zsets)} *)

(** [zadd k s m c] adds member [m] with score [s] to the sorted set at key [k] on connection [c], as per the [ZADD] redis keyword.
    @return [true] if a new element was added, [false] otherwise.
*)
val zadd : string -> float -> string -> Connection.t -> bool

(** [zrem k m c] removed member [m] from the sorted set at key [k] on connection [c], as per the [ZREM] redis keyword.
    @return [false] if the member was not part of the set, [true] otherwise.
*)
val zrem : string -> string -> Connection.t -> bool

(** [zincrby k i m c] increment member [m] of set at key [k] by increment [i] on connection [c], as per the [ZINCRBY] redis keyword. *)
val zincrby : string -> float -> string -> Connection.t -> float

(** [zrange k s e c] returns an in-order list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZRANGE] redis keyword. *)
val zrange :
  string ->
  int -> int -> Connection.t -> bulk_data list

(** [zrevrange k s e c] returns a {i reversed ordered} list of members of the sorted set at key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZREVRANGE] redis keyword. *)
val zrevrange :
  string ->
  int -> int -> Connection.t -> bulk_data list

(** [zrangebyscore k min max limit c] returns a list of all the members in sorted set at the key [k] with scores between [min] and [max], inclusively, on connection [c], as per the [ZRANGEBYSCORE] redis keyword.
    @param limit Pass in [`Limit(offset, limit)] to limit the number of returned values by [limit] offset by [offset].
*)
val zrangebyscore :
  string ->
  float ->
  float ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  Connection.t -> bulk_data list

(** [zcard k c] returns the number of members in the sorted set at the key [k] on connection [c], as per the [ZCARD] redis keyword. *)
val zcard : string -> Connection.t -> int

(** [zscore k e c] returns the score of the element [e] of the sorted set at the key [k] on connection [c], as per the [ZSCORE] redis keyword. *)
val zscore : string -> string -> Connection.t -> float

(** [zremrangebyscore k min max] removes all the memebers of the sorted set at the key [k] with scores between [min] and [max] on connection [c], as per the [ZREMRANGEBYSCORE] redis keyword.
    @return the number of elements removed.
*)
val zremrangebyscore : string -> float -> float -> Connection.t -> int

(** {3:sort_cmd Sorting} *)

(** [sort k pattern limit get order alpha c] returns the members of a list, set or sorted set at key [k] on connection [c], as per the [sort] redis keyword.
    @param pattern key pattern (i.e. [weight_*]) to use to fetch the value to sort by.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param get instead of returning the values in the key [k], use this pattern (i.e. [object_*]) to produce the output.
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort :
  string ->
  ?pattern:string ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  ?get:string ->
  ?order:[< `Asc | `Desc > `Asc ] ->
  ?alpha:[< `Alpha | `NonAlpha > `NonAlpha ] ->
  Connection.t -> bulk_data list

(** {3:persistence_cmd Persistence control commands} *)

(** [save c] synchronously save the DB to disk on connection [c], as per the [SAVE] redis keyword. *)
val save : Connection.t -> unit

(** [bgsave c] asynchronously save the DB to disk on connection [c], as per the [BGSAVE] redis keyword. *)
val bgsave : Connection.t -> unit

(** [lastsave c] returns the UNIX time of the last save on connection [c], as per the [LASTSAVE] redis keyword.
    @return a float, because the unix time is bigger than the int size in ocaml on 32 bit architectures.
*)
val lastsave : Connection.t -> float

(** [shutdown c] write to disk and then shuts down the server on connection [c], as per the [SHUTDOWN] redis keyword. *)
val shutdown : Connection.t -> unit

(** [bgrewriteaof c] rewrite the append only file in the background on connection [c], as per the [BGREWRITEAOF] redis keyword. *)
val bgrewriteaof : Connection.t -> unit

(** {3:remote_cmd Remote server control commands} *)

(** The [Info] module is used to manage a type containing redis server information. *)
module Info :
    sig
        (** Container for redis server information. *)
        type t = {fields: string list; values: (string, string) Hashtbl.t;}

        (** [get i f] returns the value associated to the field [f] in the {!Info.t} container [t]. *)
        val get : t -> string -> string

        (** [get_fields i] returns a list of all the fields contained in the {!Info.t} container [t]. *)
        val get_fields : t -> string list
    end

(** [info c] returns information about the redis server, as per the [INFO] redis keyword. See {!Info} for more information about how to manipulate the container this function returns.
*)
val info : Connection.t -> Info.t
