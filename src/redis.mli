(**
Redis is a module used to interact with a redis-key value store server. For a full description of all the redis keywords, refer to the redis docs linked below.

@see <http://code.google.com/p/redis/> the redis project.

Note that every command below will raise a {!RedisServerError} exception if the redis server sends back some sort of error.
*)

(** {3:redis_types Types used with redis} *)

(** Different types of redis keys, as per the TYPE keyword. To get a string representation, use {!string_of_redis_value_type}. *)
type redis_value_type = RedisString | RedisNil | RedisList | RedisSet | RedisZSet

(** Bulk types. To get a string representation, use {!string_of_bulk_data}. *)
type bulk_data = Nil | String of string

(** Exception raised when trying to get a {!bulk_data} [String] out of a {!bulk_data} [Nil] type. *)
exception RedisNilError of string

(** Gives a string representation of a {!redis_value_type} type. *)
val string_of_redis_value_type : redis_value_type -> string

(** Returns the string contained by a {!bulk_data} type, as long as the type is {!bulk_data} [String]. If it is {!bulk_data} [Nil], it raises a {!RedisNilError} exception. *)
val string_of_bulk_data : bulk_data -> string

(** Exception when getting an error ("-...") response from the redis server. *)
exception RedisServerError of string

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

(** [randomkey c] returns a random key on connection [c], as per the [RANDOMKEY] redis keyword. If no keys are in the store, will raise a RedisNilError. *)
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

(** [expireat k u c] sets the expire time of key [k] to the time [u] on connection [c], as per the [EXPIRE] redis keyword.
    @param u A time as a unix time stamp, i.e. the number of seconds since January 1st, 1970 UTC).
*)
val expireat :string -> float -> Connection.t -> bool

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

(** [append k v c] appends string [v] to key [k] on connection [c], as per the [APPEND] redis keyword.
    @return the new length of the value stored at that key.
*)
val append : string -> string -> Connection.t -> int

(** {3:list_cmd Commands operating on lists} *)

(** [rpush k v c] pushes value [v] to the tail of the list at key [k] on connection [c], as per the [RPUSH] redis keyword.
    @return the number of elements in the list after the push
*)
val rpush : string -> string -> Connection.t -> int

(** [lpush k v c] pushes value [v] to the head of the list at key [k] on connection [c], as per the [LPUSH] redis keyword.
    @return the number of elements in the list after the push
*)
val lpush : string -> string -> Connection.t -> int

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

(** [blpop k timeout c] blocks until it can pop a value off the list at [k] on connection [c], as per the [BLPOP] redis keyword, but for only one key.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return the value poped from key [k], or {!bulk_data} [Nil] if the list at [k] is empty
*)
val blpop :
    string ->
    ?timeout:[< `None | `Seconds of int > `None ] ->
    Connection.t ->
    bulk_data

(** [blpop_many kl timeout c] blocks until it can pop a value off one of the lists given by the list of keys [kl] on connection [c], as per the [BLPOP] redis keyword, like {!blpop} but for many keys.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return a pair of ([key], {!bulk_data}), which is the value popped from the list at [key]. If no value was found before the timeout, a pair ([""], {!bulk_data} [Nil]) is returned
*)
val blpop_many :
    string list ->
    ?timeout:[< `None | `Seconds of int > `None ] ->
    Connection.t ->
    string * bulk_data

(** [brpop k timeout c] blocks until it can pop a value off the list at [k] on connection [c], as per the [BRPOP] redis keyword, but for only one key.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return the value poped from key [k], or {!bulk_data} [Nil] if the list at [k] is empty
*)
val brpop :
    string ->
    ?timeout:[< `None | `Seconds of int > `None ] ->
    Connection.t ->
    bulk_data

(** [brpop_many kl timeout c] blocks until it can pop a value off one of the lists given by the list of keys [kl] on connection [c], as per the [BRPOP] redis keyword, like {!brpop} but for many keys.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return a pair of ([key], {!bulk_data}), which is the value popped from the list at [key]. If no value was found before the timeout, a pair ([""], {!bulk_data} [Nil]) is returned
*)
val brpop_many :
    string list ->
    ?timeout:[< `None | `Seconds of int > `None ] ->
    Connection.t ->
    string * bulk_data

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

(** [sdiff fk kl c] returns the difference between the set at the from key [km] and the other keys in the [kl] on connection [c], as per the [SDIFF] redis keyword. *)
val sdiff :
  string -> string list -> Connection.t -> bulk_data list

(** [sdiffstore dk fk kl c] puts the difference between the set at the from key [fk] and the other keys in the [kl] into the set at the destination key [dk] on connection [c], as per the [SDIFFSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sdiffstore : string -> string -> string list -> Connection.t -> int

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

(** [zrange_withscores k s e c], returns an in-order list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c]. This is exactly like the {!zrange} function except it also gives the score for each item.
    @return a list of [({bulk_data}, float)] for the specified range.
*)
val zrange_withscores : 
  string ->
  int -> int -> Connection.t -> (bulk_data * float) list

(** [zrevrange k s e c] returns a {i reversed ordered} list of members of the sorted set at key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZREVRANGE] redis keyword. *)
val zrevrange :
  string ->
  int -> int -> Connection.t -> bulk_data list

(** [zrevrange_withscores k s e c], returns a {i reversed ordered} list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c]. This is exactly like the {!zrevrange} function except it also gives the score for each item.
    @return a list of [({bulk_data}, float)] for the specified range.
*)
val zrevrange_withscores : 
  string ->
  int -> int -> Connection.t -> (bulk_data * float) list

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

(** {3:hash_cmd Commands operating on hashes} *)

(** [hset k f v c] sets field [f] to value [v] at key [k] on connection [c], as per the [HSET] redis keyword.
    @return [true] if the field does not already exist in the hash
*)
val hset : string -> string -> string -> Connection.t -> bool

(** [hdel k f c] deletes field [f] from key [k] on connection [c], as per the [HDEL] redis keyword.
    @return [true] if the field existed and was deleted, [false] otherwise
*)
val hdel : string -> string -> Connection.t -> bool

(** [hget k f c] retrieves the string stored for field [f] at key [k] on connection [c], as per the [HGET] redis keyword.
    @return {!Redis.Nil} if the field or the key cannot be found.
*)
val hget : string -> string -> Connection.t -> bulk_data

(** [hmget k fl c] retrieves the list of fields [fl] off key [k] on connection [c], as per the [HMGET] redis keyword.
*)
val hmget : string -> string list -> Connection.t -> bulk_data list

(** [hmset k fv c] sets the list of field-value pairs [fv] on connection [c], as per the [HMSET] redis keyword.
*)
val hmset : string -> (string * string) list -> Connection.t -> unit

(** [hincrby k f v c] increments the field [f] on key [k] by value [v] on connection [c], as per the [HINCRBY] redis keyword.
*)
val hincrby : string -> string -> int -> Connection.t -> int

(** [hexists k f c] returns [true] if field [f] exists at key [k] on connection [c], [false] otherwise, as per the [HEXISTS] redis keyword.
*)
val hexists : string -> string -> Connection.t -> bool

(** [hlen k c] returns the number of fields at key [k] on connection [c], as per the [HLEN] redis keyword.
*)
val hlen : string -> Connection.t -> int

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

(** [sort_get_many k gt pattern limit order alpha c] sorts a list, set or sorted set at key [k] on connection [c], as per the [SORT] redis keyword. This function is a way to use the [GET] keyword multiple times as per the redis spec and collate them into one list while not dealing with wrapping and unwrapping values from lists in the simplest case with {!sort}.
    @return a list of lists of values "gotten" by the patterns in the list [gt]
    @param pattern key pattern (i.e. [weight_*]) to use to fetch the value to sort by.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort_get_many :
  string ->
  string list ->
  ?pattern:string ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  ?order:[< `Asc | `Desc > `Asc ] ->
  ?alpha:[< `Alpha | `NonAlpha > `NonAlpha ] ->
  Connection.t -> bulk_data list list

(** [sort_and_store k gt d pattern limit order alpha c] sorts a list, set or sorted set at key [k] on connection [c], and places the results at key [d], as per the [SORT] redis keyword. This function is a way to use the [STORE] keyword with either one or multiple [GET]s times as per the redis spec.
    @return the length of the destination list at key [d]
    @param gt a list of keys to store in the destination list, can be a list with one item.
    @param pattern key pattern (i.e. [weight_*]) to use to fetch the value to sort by.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort_and_store :
  string ->
  string list ->
  string ->
  ?pattern:string ->
  ?limit:[< `Limit of int * int | `Unlimited > `Unlimited ] ->
  ?order:[< `Asc | `Desc > `Asc ] ->
  ?alpha:[< `Alpha | `NonAlpha > `NonAlpha ] ->
  Connection.t -> int

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

(** [slaveof a p c] makes the redis server a slave of another redis server at host [a] and port [o] on connection [c], as per the [SLAVEOF] redis keyword. *)
val slaveof : string -> int -> Connection.t -> unit
