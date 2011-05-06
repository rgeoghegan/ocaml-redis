(**
Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
Released under the BSD license. See the LICENSE.txt file for more info.

Redis is a module used to interact with a redis-key value store server. For a full description of all the redis keywords, refer to the redis docs linked below.

@see <http://code.google.com/p/redis/> the redis project.

Note that every command below will raise a {!RedisServerError} exception if the redis server sends back some sort of error.
*)

(** {3:redis_types Types used with redis} *)

(** Different types of redis keys, as per the TYPE keyword. To get a string representation, use {!Value.to_string}. *)

module Value : sig 

  type t = 
    | Nil 
    | String 
    | List 
    | Set 
    | SortedSet

  type one = string option
  type pair = (string * string) option
  type many = one list option

  val to_string : t -> string
  val get : 'a option -> 'a 

end

(** Rank types. Ranks are an integer, or [Nil] for a non-existant key. *)
type rank = NilRank | Rank of int

type timeout = Seconds of int | Wait

type limit = Unlimited | Limit of int * int 

type aggregate = Min | Max | Sum

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
val auth : Connection.t -> string -> unit

(** {3:all_cmds Commands operating on all the kind of values} *)

(** [exists k c] checks if key [k] exists on connection [c], as per the [EXISTS] redis keyword. *)
val exists : Connection.t -> string -> bool

(** [del kl c] deletes all the keys in the list [kl] on connection [c], as per the [DEL] redis keyword.
    @return the number of deleted keys.
*)
val del : Connection.t -> string list -> int

(** [del_one k c] deletes the key [k] on connection [c]. This is a utility function to avoid creating a list of one key with the {!del} function.
    @return [true] if the key was deleted.
*)
val del_one : Connection.t -> string -> bool

(** [value_type k c] returns the type of key [k] on connection [c], as per the [TYPE] redis keyword. Note that this is not named type because [type] is a reserved keyword in Ocaml. *)
val value_type : Connection.t -> string -> Value.t

(** [keys p c] returns a list of keys matching the pattern [p] on connection [c], as per the [KEYS] redis keyword. *)
val keys : Connection.t -> string -> Value.many

(** [randomkey c] returns a random key on connection [c], as per the [RANDOMKEY] redis keyword. If no keys are in the store, will raise a RedisNilError. *)
val randomkey : Connection.t -> Value.one

(** [rename on nn c] renames old key name [on] to new key name [nn] on connection [c], as per the [RENAME] redis keyword. *)
val rename : Connection.t -> string -> string -> unit

(** [renamenx on nn c] renames old key name [on] to new key name [nn] on connection [c], much like {!rename}, except it will return [true] if the new key does not already exists, [false] otherwise, as per the [RENAMENX] redis keyword. *)
val renamenx : Connection.t -> string -> string -> bool

(** [dbsize c] returns the number of keys in the database *)
val dbsize : Connection.t -> int

(** [expire k s c] sets the expire time of key [k] to [s] seconds on connection [c], as per the [EXPIRE] redis keyword.
    @return [true] if the timeout was set, false if the key does not exist or already has an associated timeout.
*)
val expire : Connection.t -> string -> int -> bool

(** [expireat k u c] sets the expire time of key [k] to the time [u] on connection [c], as per the [EXPIRE] redis keyword.
    @param u A time as a unix time stamp, i.e. the number of seconds since January 1st, 1970 UTC).
*)
val expireat : Connection.t -> string -> float -> bool

(** [ttl k c] returns the time to live in seconds for key [k] on connection [c], as per the [TTL] redis keyword. *)
val ttl : Connection.t -> string -> int

(** [select i c] selects database index [i] on connection [c], as per the [SELECT] redis keyword. *)
val select : Connection.t -> int -> unit

(** [move k db c] moves key [k] to database index [db] on connection [c], as per the [MOVE] redis keyword.
    @return [false] if the key [k] exists in the database [db] or does not exist in the current database.
*)
val move : Connection.t -> string -> int -> bool

(** [flushdb c] flushes all the keys from the currently selected database, as per the [FLUSHDB] redis keyword. *)
val flushdb : Connection.t -> unit

(** [flushall c] flushes all the keys from all the databases, as per the [FLUSHALL] redis keyword. *)
val flushall : Connection.t -> unit

(** {3:string_cmd Commands operating on string values} *)

(** [set k v c] sets key [k] to value [v] on connection [c], as per the [SET] redis keyword. *)
val set : Connection.t -> string -> string -> unit

(** [get k c] gets the key [k] on connection [c], as per the [GET] redis keyword. *)
val get : Connection.t -> string -> Value.one

(** [getset k v c] gets the key [k] on connection [c], and then sets it to [v], as per the [GETSET] redis keyword. *)
val getset : Connection.t -> string -> string -> Value.one

(** [mget kl c] gets the values associated to each key in list [kl] on connection [c], as per the [MGET] redis keyword. *)
val mget :  Connection.t -> string list -> Value.many

(** [setnx k v c] sets key [k] to value [v] on connection [c], as per the [SETNX] redis keyword. As opposed to {!set}, will return [false] and not set the key if the key [k] already exists; otherwise returns [true]. *)
val setnx : Connection.t -> string -> string -> bool

(** [setex k t v c] sets the key [k] to value [v] with timeout [t] on connection [c], as per the [SETEX] redis keyword.
*)
val setex : Connection.t -> string -> int -> string -> unit

(** [mset kv c] sets the list of key-value pairs [kv] on connection [c], as per the [MSET] redis keyword. *)
val mset : Connection.t -> (string * string) list -> unit

(** [msetnx kv c] sets the list of key-value pairs [kv] on connection [c], as per the [MSETNX] redis keyword. As opposed to {!mset}, will return [false] and not set {i any} of the given keys if any of them exist already. *)
val msetnx : Connection.t -> (string * string) list -> bool

(** [incr k c] increments key [k] by 1 on connection [c], as per the [INCR] redis keyword.
    @return the new value of the key.
*)
val incr : Connection.t -> string -> int

(** [incrby k i c] increments key [k] by interger [i] on connection [c], as per the [INCRBY] redis keyword.
    @return the new value of the key.
*)
val incrby : Connection.t -> string -> int -> int

(** [decr k c] decrements key [k] by 1 on connection [c], as per the [DECR] redis keyword.
    @return the new value of the key.
*)
val decr : Connection.t -> string -> int

(** [decrby k i c] decrements key [k] by interger [i] on connection [c], as per the [DECRBY] redis keyword.
    @return the new value of the key.
*)
val decrby : Connection.t -> string -> int -> int

(** [append k v c] appends string [v] to key [k] on connection [c], as per the [APPEND] redis keyword.
    @return the new length of the value stored at that key.
*)
val append : Connection.t -> string -> string -> int

(** [substr k s e c] returns the substring starting at index [s] and ending at index [e] for key [k] on connection [c].
*)
val substr : Connection.t -> string -> int -> int -> Value.one

(** {3:list_cmd Commands operating on lists} *)

(** [rpush k v c] pushes value [v] to the tail of the list at key [k] on connection [c], as per the [RPUSH] redis keyword.
    @return the number of elements in the list after the push
*)
val rpush : Connection.t -> string -> string -> int

(** [lpush k v c] pushes value [v] to the head of the list at key [k] on connection [c], as per the [LPUSH] redis keyword.
    @return the number of elements in the list after the push
*)
val lpush : Connection.t -> string -> string -> int

(** [llen k c] returns the length of list as key [k] on connection [c], as per the [LLEN] redis keyword. *)
val llen : Connection.t -> string -> int

(** [lrange k s e c] returns the elements of list as key [k] between start index [s] and end index [e] inclusively on connection [c], as per the [LRANGE] redis keyword. *)
val lrange : Connection.t -> string -> int -> int -> Value.many

(** [ltrim k s e c] remove all elements of the list at key [k] {i not} between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ltrim] redis keyword. *)
val ltrim : Connection.t -> string -> int -> int -> unit

(** [lindex k i c] get the value at index [i] in the list at key [k] on connection [c], as per the [LINDEX] redis keyword. *)
val lindex : Connection.t -> string -> int -> Value.one

(** [lset k i v c] sets the index [i] of the list at key [k] to value [v] on connection [c], as per the [LSET] redis keyword. *)
val lset : Connection.t -> string -> int -> string -> unit

(** [lrem k ct v c] removes up to count [ct] values [v] from list at key [k] on connection [c], as per the [LREM] redis keyword.
    @return the number of values removed.
*)
val lrem : Connection.t -> string -> int -> string -> int

(** [lpop k c] pops the head of the list at key [k] on connection [c], as per the [LPOP] redis keyword.
    @return the value popped.
*)
val lpop : Connection.t -> string -> Value.one

(** [rpop k c] pops the tail of the list at key [k] on connection [c], as per the [RPOP] redis keyword.
    @return the value popped.
*)
val rpop : Connection.t -> string -> Value.one

(** [blpop k timeout c] blocks until it can pop a value off the list at [k] on connection [c], as per the [BLPOP] redis keyword, but for only one key.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return the value poped from key [k], or {!string} [Nil] if the list at [k] is empty
*)
val blpop : Connection.t -> ?timeout:timeout -> string -> Value.pair

(** [blpop_many kl timeout c] blocks until it can pop a value off one of the lists given by the list of keys [kl] on connection [c], as per the [BLPOP] redis keyword, like {!blpop} but for many keys.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return a pair of ([key], {!string}), which is the value popped from the list at [key]. If no value was found before the timeout, a pair ([""], {!string} [Nil]) is returned
*)
val blpop_many : Connection.t -> ?timeout:timeout -> string list -> Value.pair

(** [brpop k timeout c] blocks until it can pop a value off the list at [k] on connection [c], as per the [BRPOP] redis keyword, but for only one key.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return the value poped from key [k], or {!string} [Nil] if the list at [k] is empty
*)
val brpop : Connection.t -> ?timeout:timeout -> string -> Value.pair

(** [brpop_many kl timeout c] blocks until it can pop a value off one of the lists given by the list of keys [kl] on connection [c], as per the [BRPOP] redis keyword, like {!brpop} but for many keys.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return a pair of ([key], {!string}), which is the value popped from the list at [key]. If no value was found before the timeout, a pair ([""], {!string} [Nil]) is returned
*)
val brpop_many : Connection.t -> ?timeout:timeout -> string list -> Value.pair

(** [rpoplpush sk dk c] pops the tail of the list at the source key [sk] and pushes it to the tail of the list at the destination key [dk] on connection [c], as per the [RPOPLPUSH] redis keyword. *)
val rpoplpush : Connection.t -> string -> string -> Value.one

(** {3:set_cmd Commands operating on sets} *)

(** [sadd k m c] add member [m] to set at key [k] on connection [c], as per the [SADD] redis keyword.
    @return [true] if member [m] not part of the set already, [false] otherwise.
*)
val sadd : Connection.t -> string -> string -> bool

(** [srem k m c] removes member [m] from the set at key [k] on connection [c], as per the [SREM] redis keyword.
    @return [true] if member [m] was part of the set, [false] otherwise.
*)
val srem : Connection.t -> string -> string -> bool

(** [spop k c] pop a random member from the set at key [k] on connection [c], as per the [SPOP] redis keyword.
    @return the member that got popped.
*)
val spop : Connection.t -> string -> Value.one

(** [smove sk dk m c] moves the member [m] from the set at the source key [sk] to the set at the destination key [dk] on connection c, as per the [SMOVE] redis keyword.
    @return [false] if the element was not found in the first set and no operation was done, [false] otherwise. 
*)
val smove : Connection.t -> string -> string -> string -> bool

(** [scard k c] returns the number of members in the set at key [k] on connection [c], as per the [SCARD] redis keyword. *)
val scard : Connection.t -> string -> int

(** [sismember k m c] checks if the member [m] is in the set at key [k] exists on connection [c], as per the [SISMEMBER] redis keyword. *)
val sismember : Connection.t -> string -> string -> bool

(** [sinter kl c] returns the intersection of all the sets at the keys listed in [kl] on connection [c], as per the [SINTER] redis keyword. *)
val sinter : Connection.t -> string list -> Value.many

(** [sinterstore dk kl c] puts the intersection of all the sets listed in [kl] into the set at destination key [dk] on connection [c], as per the [SINTERSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sinterstore : Connection.t -> string -> string list -> int

(** [sunion kl c] returns the union of all the sets at the keys listed in [kl] on connection [c], as per the [SUNION] redis keyword. *)
val sunion : Connection.t -> string list -> Value.many

(** [sunionstore dk kl c] puts the union of all the sets listed in [kl] into the set at destination key [dk] on connection [c], as per the [SUNIONSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sunionstore : Connection.t -> string -> string list -> int

(** [sdiff fk kl c] returns the difference between the set at the from key [km] and the other keys in the [kl] on connection [c], as per the [SDIFF] redis keyword. *)
val sdiff : Connection.t -> string -> string list -> Value.many

(** [sdiffstore dk fk kl c] puts the difference between the set at the from key [fk] and the other keys in the [kl] into the set at the destination key [dk] on connection [c], as per the [SDIFFSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sdiffstore : Connection.t -> string -> string -> string list -> int

(** [smembers k c] returns all the members of the set at key [k] on connection [c], as per the [SMEMBERS] redis keyword. *)
val smembers : Connection.t -> string -> Value.many

(** [srandmember k c] returns a random member from the set at the key [k] on connection [c], as per the [SRANDMEMBER] redis keyword. *)
val srandmember : Connection.t -> string -> Value.one

(** {3:sorted_sets_cmd Commands operating on sorted sets (zsets)} *)

(** [zadd k s m c] adds member [m] with score [s] to the sorted set at key [k] on connection [c], as per the [ZADD] redis keyword.
    @return [true] if a new element was added, [false] otherwise.
*)
val zadd : Connection.t -> string -> float -> string -> bool

(** [zrem k m c] removed member [m] from the sorted set at key [k] on connection [c], as per the [ZREM] redis keyword.
    @return [false] if the member was not part of the set, [true] otherwise.
*)
val zrem : Connection.t -> string -> string -> bool

(** [zincrby k i m c] increment member [m] of set at key [k] by increment [i] on connection [c], as per the [ZINCRBY] redis keyword. *)
val zincrby : Connection.t -> string -> float -> string -> float

(** [zrank k m c] returns the rank of member [m] at key [k] on connection [c], as per the [ZRANK] redis keyword. *)
val zrank : Connection.t -> string -> string -> rank

(** [zrevrank k m c] returns the reverse rank of member [m] at key [k] on connection [c], as per the [ZREVRANK] redis keyword. *)
val zrevrank : Connection.t -> string -> string -> rank

(** [zrange k s e c] returns an in-order list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZRANGE] redis keyword. *)
val zrange : Connection.t -> string -> int -> int -> Value.many

(** [zrange_withscores k s e c], returns an in-order list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c]. This is exactly like the {!zrange} function except it also gives the score for each item.
    @return a list of [({!string}, float)] for the specified range.
*)
val zrange_with_scores : Connection.t -> string -> int -> int -> (string * float) option list option

(** [zrevrange k s e c] returns a {i reversed ordered} list of members of the sorted set at key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZREVRANGE] redis keyword. *)
val zrevrange : Connection.t -> string -> int -> int -> Value.many

(** [zrevrange_withscores k s e c], returns a {i reversed ordered} list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c]. This is exactly like the {!zrevrange} function except it also gives the score for each item.
    @return a list of [({string}, float)] for the specified range.
*)
val zrevrange_with_scores : Connection.t -> string -> int -> int -> (string * float) option list option

(** [zrangebyscore k min max limit c] returns a list of all the members in sorted set at the key [k] with scores between [min] and [max], inclusively, on connection [c], as per the [ZRANGEBYSCORE] redis keyword.
    @param limit Pass in [`Limit(offset, limit)] to limit the number of returned values by [limit] offset by [offset].
*)
val zrangebyscore :
  Connection.t -> 
  ?limit:limit ->
  string ->
  float ->
  float -> Value.many

(** [zcard k c] returns the number of members in the sorted set at the key [k] on connection [c], as per the [ZCARD] redis keyword. *)
val zcard : Connection.t -> string -> int

(** [zscore k e c] returns the score of the element [e] of the sorted set at the key [k] on connection [c], as per the [ZSCORE] redis keyword. *)
val zscore : Connection.t -> string -> string -> float

(** [zremrangebyrank k s e c] removes all the members of key [k] on connection [c] in the ranks starting at [s] and ending at [e], as per the [ZREMRANGEBYRANK] redis keyword.
    @return the number of deleted members.
*)
val zremrangebyrank : Connection.t -> string -> int -> int -> int

(** [zremrangebyscore k min max] removes all the memebers of the sorted set at the key [k] with scores between [min] and [max] on connection [c], as per the [ZREMRANGEBYSCORE] redis keyword.
    @return the number of elements removed.
*)
val zremrangebyscore : Connection.t -> string -> float -> float -> int

(** [zunionstore d kl aggregate c] stores the union of all the members in the sorted sets at [kl] in destination key [d] by aggregating by [aggregate] on connection [c], as per the [ZUNIONSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zunionstore : Connection.t -> ?aggregate:aggregate -> string -> string list -> int

(** [zunionstore_with_weights d kl wl aggregate c] stores the union of all the members in the sorted sets at [kl] in destination key [d] by first multiplying the scores in each key by wl and then by aggregating by [aggregate] on connection [c], as per the [ZUNIONSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zunionstore_with_weights : Connection.t -> ?aggregate:aggregate -> string -> string list -> float list -> int

(** [zinterstore d kl aggregate c] stores the intersection of all the members in the sorted sets at [kl] in destination key [d] by aggregating by [aggregate] on connection [c], as per the [ZINTERSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zinterstore : Connection.t -> ?aggregate:aggregate -> string -> string list -> int

(** [zinterstore_with_weights d kl wl aggregate c] stores the intersection of all the members in the sorted sets at [kl] in destination key [d] by first multiplying the scores in each key by wl and then by aggregating by [aggregate] on connection [c], as per the [ZINTERSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zinterstore_with_weights : Connection.t -> ?aggregate:aggregate -> string -> string list -> float list -> int

(** {3:hash_cmd Commands operating on hashes} *)

(** [hset k f v c] sets field [f] to value [v] at key [k] on connection [c], as per the [HSET] redis keyword.
    @return [true] if the field does not already exist in the hash
*)
val hset : Connection.t -> string -> string -> string -> bool

(** [hdel k f c] deletes field [f] from key [k] on connection [c], as per the [HDEL] redis keyword.
    @return [true] if the field existed and was deleted, [false] otherwise
*)
val hdel : Connection.t -> string -> string -> bool

(** [hget k f c] retrieves the string stored for field [f] at key [k] on connection [c], as per the [HGET] redis keyword.
    @return {!string} [Nil] if the field or the key cannot be found.
*)
val hget : Connection.t -> string -> string -> Value.one

(** [hmget k fl c] retrieves the list of fields [fl] off key [k] on connection [c], as per the [HMGET] redis keyword.
*)
val hmget : Connection.t -> string -> string list -> Value.many

(** [hmset k fv c] sets the list of field-value pairs [fv] on connection [c], as per the [HMSET] redis keyword.
*)
val hmset : Connection.t -> string -> (string * string) list -> unit

(** [hincrby k f v c] increments the field [f] on key [k] by value [v] on connection [c], as per the [HINCRBY] redis keyword.
*)
val hincrby : Connection.t -> string -> string -> int -> int

(** [hexists k f c] returns [true] if field [f] exists at key [k] on connection [c], [false] otherwise, as per the [HEXISTS] redis keyword.
*)
val hexists : Connection.t -> string -> string -> bool

(** [hlen k c] returns the number of fields at key [k] on connection [c], as per the [HLEN] redis keyword.
*)
val hlen : Connection.t -> string -> int

(** [hkeys k c] returns a list of all the fields at key [k] on connection [c], as per the [HKEYS] redis keyword.
*)
val hkeys : Connection.t -> string -> Value.many

(** [hvals k c] returns a list of all the values tied to fields at key [k] on connection [c], as per the [HVALS] redis keyword.
*)
val hvals : Connection.t -> string -> Value.many

(** [hgetall k c] returns the pairs of field/values stored at key [k] on connection [c], as per the [HGETALL] redis keyword.
*)
val hgetall : Connection.t -> string -> (string * string) list

(** {3:sort_cmd Sorting} *)

(** Sorting sometimes requires a pattern to be specifies. [KeyPattern(pattern] specifies the pattern for a key, [FieldPattern(key pattern)] specifies a specific field from a hash following the key pattern, [NoSort] specifies to not perform any sorting, and [NoPattern] is used to cancel the pattern parameter, and is usually a default.
*)
type redis_sort_pattern = KeyPattern of string | FieldPattern of string * string | NoSort | NoPattern

type sort_order = Asc | Desc

type sort_alpha = Alpha | NonAlpha

(** [sort k pattern limit get order alpha c] returns the members of a list, set or sorted set at key [k] on connection [c], as per the [sort] redis keyword.
    @param pattern a {!redis_sort_pattern}, [NoPattern] by default.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param get instead of returning the values in the key [k], use a pattern (i.e. [object_*]) to produce the output.
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort :
  Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?get:redis_sort_pattern ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string -> Value.many

(** [sort_get_many k gt pattern limit order alpha c] sorts a list, set or sorted set at key [k] on connection [c], as per the [SORT] redis keyword. This function is a way to use the [GET] keyword multiple times as per the redis spec and collate them into one list while not dealing with wrapping and unwrapping values from lists in the simplest case with {!sort}.
    @return a list of lists of values "gotten" by the patterns in the list [gt]
    @param pattern a {!redis_sort_pattern}, [NoPattern] by default.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort_get_many :
  Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string ->
  string list -> Value.many list

(** [sort_and_store k gt d pattern limit order alpha c] sorts a list, set or sorted set at key [k] on connection [c], and places the results at key [d], as per the [SORT] redis keyword. This function is a way to use the [STORE] keyword with either one or multiple [GET]s times as per the redis spec.
    @return the length of the destination list at key [d]
    @param gt a list of keys to store in the destination list, can be a list with one item.
    @param pattern a {!redis_sort_pattern}, [NoPattern] by default.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort_and_store :
  Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string ->
  string list ->
  string -> int

(** {3:persistence_cmd Persistence control commands} *)

(** [save c] synchronously save the DB to disk on connection [c], as per the [SAVE] redis keyword. *)
val save : Connection.t -> unit

(** [bgsave c] asynchronously save the DB to disk on connection [c], as per the [BGSAVE] redis keyword. *)
val bgsave : Connection.t -> unit

(** [lastsave c] returns the UNIX time of the last save on connection [c], as per the [LASTSAVE] redis keyword.
    @return a float, because the unix time is bigger than the int size in ocaml on 32 bit architectures.
*)
val lastsave : Connection.t -> int64

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
val slaveof : Connection.t -> string -> int -> unit

