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

  val to_string : t -> string
  val get : 'a option -> 'a 

end

type timeout = Seconds of int | Wait

type limit = Unlimited | Limit of int * int 

type aggregate = Min | Max | Sum

(** Exception when getting an error ("-...") response from the redis server. *)
exception RedisServerError of string

(** {3:connection Connection handling} *)

(** The [Connection] module is used to abstract away all the socket manipulations done by the {!Redis} module. *)
module Connection : sig
  (** This type is used to represent the connection to be passed around to all the functions. *)
  type 'a t

end

module Pipeline : sig 

  (* buffer commands from this point on *)
  val enable : 'a Connection.t -> unit 

  (* receive and invoke continuations *)
  val receive : 'a Connection.t -> 'a -> 'a
    
end

(** Returns a {!Connection.t} to be used by all the {!Redis} functions.
    @param addr Address of the redis server. Defaults to the localhost ([127.0.0.1]).
    @param port Port of the redis server. Defaults to the default redis port ([6379]).
*)
val create_connection : ?addr:string -> ?port:int -> unit -> 'a Connection.t

(** Ping the server to see if it is up, as per the [PING] redis keyword. *)
val ping : 'a Connection.t -> unit

val pping : 'a Connection.t -> unit

(** Close connection to redis, as per the [QUIT] redis keyword. *)
val quit : 'a Connection.t -> unit

(** [auth p c] authenticates connection [c] with password [p], as per the [AUTH] redis keyword. *)
val auth : 'a Connection.t -> string -> unit

val pauth : 'a Connection.t -> string -> unit

(** {3:all_cmds Commands operating on all the kind of values} *)

(** [exists k c] checks if key [k] exists on connection [c], as per the [EXISTS] redis keyword. *)
val exists : 'a Connection.t -> string -> bool

val pexists : 'a Connection.t -> string -> ('a -> bool -> 'a) -> unit

(** [del kl c] deletes all the keys in the list [kl] on connection [c], as per the [DEL] redis keyword.
    @return the number of deleted keys.
*)
val del : 'a Connection.t -> string list -> int

val pdel : 'a Connection.t -> string list -> ('a -> int -> 'a) -> unit

(** [del_one k c] deletes the key [k] on connection [c]. This is a utility function to avoid creating a list of one key with the {!del} function.
    @return [true] if the key was deleted.
*)
val del_one : 'a Connection.t -> string -> bool

val pdel_one : 'a Connection.t -> string -> ('a -> bool -> 'a) -> unit

(** [value_type k c] returns the type of key [k] on connection [c], as per the [TYPE] redis keyword. Note that this is not named type because [type] is a reserved keyword in Ocaml. *)
val value_type : 'a Connection.t -> string -> Value.t

val pvalue_type : 'a Connection.t -> string -> ('a -> Value.t -> 'a) -> unit

(** [keys p c] returns a list of keys matching the pattern [p] on connection [c], as per the [KEYS] redis keyword. *)
val keys : 'a Connection.t -> string -> string list

val pkeys : 'a Connection.t -> string -> ('a -> string list -> 'a) -> unit

(** [randomkey c] returns a random key on connection [c], as per the [RANDOMKEY] redis keyword. If no keys are in the store, will raise a RedisNilError. *)
val randomkey : 'a Connection.t -> string option

val prandomkey : 'a Connection.t -> ('a -> string option -> 'a) -> unit

(** [rename on nn c] renames old key name [on] to new key name [nn] on connection [c], as per the [RENAME] redis keyword. *)
val rename : 'a Connection.t -> string -> string -> unit

val prename : 'a Connection.t -> string -> string -> unit

(** [renamenx on nn c] renames old key name [on] to new key name [nn] on connection [c], much like {!rename}, except it will return [true] if the new key does not already exists, [false] otherwise, as per the [RENAMENX] redis keyword. *)
val renamenx : 'a Connection.t -> string -> string -> bool

val prenamenx : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [dbsize c] returns the number of keys in the database *)
val dbsize : 'a Connection.t -> int

val pdbsize : 'a Connection.t -> ('a -> int -> 'a) -> unit

(** [expire k s c] sets the expire time of key [k] to [s] seconds on connection [c], as per the [EXPIRE] redis keyword.
    @return [true] if the timeout was set, false if the key does not exist or already has an associated timeout.
*)
val expire : 'a Connection.t -> string -> int -> bool

val pexpire : 'a Connection.t -> string -> int -> ('a -> bool -> 'a) -> unit

(** [expireat k u c] sets the expire time of key [k] to the time [u] on connection [c], as per the [EXPIRE] redis keyword.
    @param u A time as a unix time stamp, i.e. the number of seconds since January 1st, 1970 UTC).
*)
val expireat : 'a Connection.t -> string -> float -> bool

val pexpireat : 'a Connection.t -> string -> float -> ('a -> bool -> 'a) -> unit

(** [ttl k c] returns the time to live in seconds for key [k] on connection [c], as per the [TTL] redis keyword. *)
val ttl : 'a Connection.t -> string -> int

val pttl : 'a Connection.t -> string -> ('a -> int -> 'a) -> unit

(** [select i c] selects database index [i] on connection [c], as per the [SELECT] redis keyword. *)
val select : 'a Connection.t -> int -> unit

val pselect : 'a Connection.t -> int -> unit

(** [move k db c] moves key [k] to database index [db] on connection [c], as per the [MOVE] redis keyword.
    @return [false] if the key [k] exists in the database [db] or does not exist in the current database.
*)
val move : 'a Connection.t -> string -> int -> bool

val pmove : 'a Connection.t -> string -> int -> ('a -> bool -> 'a) -> unit

(** [flushdb c] flushes all the keys from the currently selected database, as per the [FLUSHDB] redis keyword. *)
val flushdb : 'a Connection.t -> unit

val pflushdb : 'a Connection.t -> unit

(** [flushall c] flushes all the keys from all the databases, as per the [FLUSHALL] redis keyword. *)
val flushall : 'a Connection.t -> unit

val pflushall : 'a Connection.t -> unit

(** {3:string_cmd Commands operating on string values} *)

(** [set k v c] sets key [k] to value [v] on connection [c], as per the [SET] redis keyword. *)
val set : 'a Connection.t -> string -> string -> unit

val pset : 'a Connection.t -> string -> string -> unit

(** [get k c] gets the key [k] on connection [c], as per the [GET] redis keyword. *)
val get : 'a Connection.t -> string -> string option

val pget : 'a Connection.t -> string -> ('a -> string option -> 'a) -> unit

(** [getset k v c] gets the key [k] on connection [c], and then sets it to [v], as per the [GETSET] redis keyword. *)
val getset : 'a Connection.t -> string -> string -> string option

val pgetset : 'a Connection.t -> string -> string -> ('a -> string option -> 'a) -> unit

(** [mget kl c] gets the values associated to each key in list [kl] on connection [c], as per the [MGET] redis keyword. *)
val mget :  'a Connection.t -> string list -> string option list

val pmget :  'a Connection.t -> string list -> ('a -> string option list -> 'a) -> unit

(** [setnx k v c] sets key [k] to value [v] on connection [c], as per the [SETNX] redis keyword. As opposed to {!set}, will return [false] and not set the key if the key [k] already exists; otherwise returns [true]. *)
val setnx : 'a Connection.t -> string -> string -> bool

val psetnx : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [setex k t v c] sets the key [k] to value [v] with timeout [t] on connection [c], as per the [SETEX] redis keyword.
*)
val setex : 'a Connection.t -> string -> int -> string -> unit

val psetex : 'a Connection.t -> string -> int -> string -> unit

(** [mset kv c] sets the list of key-value pairs [kv] on connection [c], as per the [MSET] redis keyword. *)
val mset : 'a Connection.t -> (string * string) list -> unit

val pmset : 'a Connection.t -> (string * string) list -> unit

(** [msetnx kv c] sets the list of key-value pairs [kv] on connection [c], as per the [MSETNX] redis keyword. As opposed to {!mset}, will return [false] and not set {i any} of the given keys if any of them exist already. *)
val msetnx : 'a Connection.t -> (string * string) list -> bool

val pmsetnx : 'a Connection.t -> (string * string) list -> ('a -> bool -> 'a) -> unit

(** [incr k c] increments key [k] by 1 on connection [c], as per the [INCR] redis keyword.
    @return the new value of the key.
*)
val incr : 'a Connection.t -> string -> int64

val pincr : 'a Connection.t -> string -> ('a -> int64 -> 'a) -> unit

(** [incrby k i c] increments key [k] by interger [i] on connection [c], as per the [INCRBY] redis keyword.
    @return the new value of the key.
*)
val incrby : 'a Connection.t -> string -> int -> int64 

val pincrby : 'a Connection.t -> string -> int -> ('a -> int64 -> 'a) -> unit

(** [decr k c] decrements key [k] by 1 on connection [c], as per the [DECR] redis keyword.
    @return the new value of the key.
*)
val decr : 'a Connection.t -> string -> int64

val pdecr : 'a Connection.t -> string -> ('a -> int64 -> 'a) -> unit

(** [decrby k i c] decrements key [k] by interger [i] on connection [c], as per the [DECRBY] redis keyword.
    @return the new value of the key.
*)
val decrby : 'a Connection.t -> string -> int -> int64

val pdecrby : 'a Connection.t -> string -> int -> ('a -> int64 -> 'a) -> unit

(** [append k v c] appends string [v] to key [k] on connection [c], as per the [APPEND] redis keyword.
    @return the new length of the value stored at that key.
*)
val append : 'a Connection.t -> string -> string -> int

val pappend : 'a Connection.t -> string -> string -> ('a -> int -> 'a) -> unit

(** [getrange k s e c] returns the substring starting at index [s] and ending at index [e] for key [k] on connection [c].
*)
val getrange : 'a Connection.t -> string -> int -> int -> string

val pgetrange : 'a Connection.t -> string -> int -> int -> ('a -> string -> 'a) -> unit

(** {3:list_cmd Commands operating on lists} *)

(** [rpush k v c] pushes value [v] to the tail of the list at key [k] on connection [c], as per the [RPUSH] redis keyword.
    @return the number of elements in the list after the push
*)
val rpush : 'a Connection.t -> string -> string -> int

val prpush : 'a Connection.t -> string -> string -> ('a -> int -> 'a) -> unit

(** [lpush k v c] pushes value [v] to the head of the list at key [k] on connection [c], as per the [LPUSH] redis keyword.
    @return the number of elements in the list after the push
*)
val lpush : 'a Connection.t -> string -> string -> int

val plpush : 'a Connection.t -> string -> string -> ('a -> int -> 'a) -> unit

(** [llen k c] returns the length of list as key [k] on connection [c], as per the [LLEN] redis keyword. *)
val llen : 'a Connection.t -> string -> int

val pllen : 'a Connection.t -> string -> ('a -> int -> 'a) -> unit

(** [lrange k s e c] returns the elements of list as key [k] between start index [s] and end index [e] inclusively on connection [c], as per the [LRANGE] redis keyword. *)
val lrange : 'a Connection.t -> string -> int -> int -> string list

val plrange : 'a Connection.t -> string -> int -> int -> ('a -> string list -> 'a) -> unit

(** [ltrim k s e c] remove all elements of the list at key [k] {i not} between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ltrim] redis keyword. *)
val ltrim : 'a Connection.t -> string -> int -> int -> unit

val pltrim : 'a Connection.t -> string -> int -> int -> unit

(** [lindex k i c] get the value at index [i] in the list at key [k] on connection [c], as per the [LINDEX] redis keyword. *)
val lindex : 'a Connection.t -> string -> int -> string option

val plindex : 'a Connection.t -> string -> int -> ('a -> string option -> 'a) -> unit

(** [lset k i v c] sets the index [i] of the list at key [k] to value [v] on connection [c], as per the [LSET] redis keyword. *)
val lset : 'a Connection.t -> string -> int -> string -> unit

val plset : 'a Connection.t -> string -> int -> string -> unit

(** [lrem k ct v c] removes up to count [ct] values [v] from list at key [k] on connection [c], as per the [LREM] redis keyword.
    @return the number of values removed.
*)
val lrem : 'a Connection.t -> string -> int -> string -> int

val plrem : 'a Connection.t -> string -> int -> string -> ('a -> int -> 'a) -> unit

(** [lpop k c] pops the head of the list at key [k] on connection [c], as per the [LPOP] redis keyword.
    @return the value popped.
*)
val lpop : 'a Connection.t -> string -> string option

val plpop : 'a Connection.t -> string -> ('a -> string option -> 'a) -> unit

(** [rpop k c] pops the tail of the list at key [k] on connection [c], as per the [RPOP] redis keyword.
    @return the value popped.
*)
val rpop : 'a Connection.t -> string -> string option

val prpop : 'a Connection.t -> string -> ('a -> string option -> 'a) -> unit

(** [blpop k timeout c] blocks until it can pop a value off the list at [k] on connection [c], as per the [BLPOP] redis keyword, but for only one key.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return the value poped from key [k], or {!string} [Nil] if the list at [k] is empty
*)
val blpop : 'a Connection.t -> ?timeout:timeout -> string -> (string * string) option

val pblpop : 'a Connection.t -> ?timeout:timeout -> string -> ('a -> (string * string) option -> 'a) -> unit

(** [blpop_many kl timeout c] blocks until it can pop a value off one of the lists given by the list of keys [kl] on connection [c], as per the [BLPOP] redis keyword, like {!blpop} but for many keys.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return a pair of ([key], {!string}), which is the value popped from the list at [key]. If no value was found before the timeout, a pair ([""], {!string} [Nil]) is returned
*)
val blpop_many : 'a Connection.t -> ?timeout:timeout -> string list -> (string * string) option

val pblpop_many : 'a Connection.t -> ?timeout:timeout -> string list -> ('a -> (string * string) option -> 'a) -> unit

(** [brpop k timeout c] blocks until it can pop a value off the list at [k] on connection [c], as per the [BRPOP] redis keyword, but for only one key.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return the value poped from key [k], or {!string} [Nil] if the list at [k] is empty
*)
val brpop : 'a Connection.t -> ?timeout:timeout -> string -> (string * string) option

val pbrpop : 'a Connection.t -> ?timeout:timeout -> string -> ('a -> (string * string) option -> 'a) -> unit

(** [brpop_many kl timeout c] blocks until it can pop a value off one of the lists given by the list of keys [kl] on connection [c], as per the [BRPOP] redis keyword, like {!brpop} but for many keys.
    @param timeout provide [`Seconds(s)] to only wait for [s] seconds, by default does not timeout
    @return a pair of ([key], {!string}), which is the value popped from the list at [key]. If no value was found before the timeout, a pair ([""], {!string} [Nil]) is returned
*)
val brpop_many : 'a Connection.t -> ?timeout:timeout -> string list -> (string * string) option

val pbrpop_many : 'a Connection.t -> ?timeout:timeout -> string list -> ('a -> (string * string) option -> 'a) -> unit

(** [rpoplpush sk dk c] pops the tail of the list at the source key [sk] and pushes it to the tail of the list at the destination key [dk] on connection [c], as per the [RPOPLPUSH] redis keyword. *)
val rpoplpush : 'a Connection.t -> string -> string -> string option

val prpoplpush : 'a Connection.t -> string -> string -> ('a -> string option -> 'a) -> unit

(** {3:set_cmd Commands operating on sets} *)

(** [sadd k m c] add member [m] to set at key [k] on connection [c], as per the [SADD] redis keyword.
    @return [true] if member [m] not part of the set already, [false] otherwise.
*)
val sadd : 'a Connection.t -> string -> string -> bool

val psadd : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [srem k m c] removes member [m] from the set at key [k] on connection [c], as per the [SREM] redis keyword.
    @return [true] if member [m] was part of the set, [false] otherwise.
*)
val srem : 'a Connection.t -> string -> string -> bool

val psrem : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [spop k c] pop a random member from the set at key [k] on connection [c], as per the [SPOP] redis keyword.
    @return the member that got popped.
*)
val spop : 'a Connection.t -> string -> string option

val pspop : 'a Connection.t -> string -> ('a -> string option -> 'a) -> unit

(** [smove sk dk m c] moves the member [m] from the set at the source key [sk] to the set at the destination key [dk] on connection c, as per the [SMOVE] redis keyword.
    @return [false] if the element was not found in the first set and no operation was done, [false] otherwise. 
*)
val smove : 'a Connection.t -> string -> string -> string -> bool

val psmove : 'a Connection.t -> string -> string -> string -> ('a -> bool -> 'a) -> unit

(** [scard k c] returns the number of members in the set at key [k] on connection [c], as per the [SCARD] redis keyword. *)
val scard : 'a Connection.t -> string -> int

val pscard : 'a Connection.t -> string -> ('a -> int -> 'a) -> unit

(** [sismember k m c] checks if the member [m] is in the set at key [k] exists on connection [c], as per the [SISMEMBER] redis keyword. *)
val sismember : 'a Connection.t -> string -> string -> bool

val psismember : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [sinter kl c] returns the intersection of all the sets at the keys listed in [kl] on connection [c], as per the [SINTER] redis keyword. *)
val sinter : 'a Connection.t -> string list -> string list

val psinter : 'a Connection.t -> string list -> ('a -> string list -> 'a) -> unit

(** [sinterstore dk kl c] puts the intersection of all the sets listed in [kl] into the set at destination key [dk] on connection [c], as per the [SINTERSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sinterstore : 'a Connection.t -> string -> string list -> int

val psinterstore : 'a Connection.t -> string -> string list -> ('a -> int -> 'a) -> unit

(** [sunion kl c] returns the union of all the sets at the keys listed in [kl] on connection [c], as per the [SUNION] redis keyword. *)
val sunion : 'a Connection.t -> string list -> string list

val psunion : 'a Connection.t -> string list -> ('a -> string list -> 'a) -> unit

(** [sunionstore dk kl c] puts the union of all the sets listed in [kl] into the set at destination key [dk] on connection [c], as per the [SUNIONSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sunionstore : 'a Connection.t -> string -> string list -> int

val psunionstore : 'a Connection.t -> string -> string list -> ('a -> int -> 'a) -> unit

(** [sdiff fk kl c] returns the difference between the set at the from key [km] and the other keys in the [kl] on connection [c], as per the [SDIFF] redis keyword. *)
val sdiff : 'a Connection.t -> string -> string list -> string list

val psdiff : 'a Connection.t -> string -> string list -> ('a -> string list -> 'a) -> unit

(** [sdiffstore dk fk kl c] puts the difference between the set at the from key [fk] and the other keys in the [kl] into the set at the destination key [dk] on connection [c], as per the [SDIFFSTORE] redis keyword.
    @return the number of members in the new set.
*)
val sdiffstore : 'a Connection.t -> string -> string -> string list -> int

val psdiffstore : 'a Connection.t -> string -> string -> string list -> ('a -> int -> 'a) -> unit

(** [smembers k c] returns all the members of the set at key [k] on connection [c], as per the [SMEMBERS] redis keyword. *)
val smembers : 'a Connection.t -> string -> string list

val psmembers : 'a Connection.t -> string -> ('a -> string list -> 'a) -> unit

(** [srandmember k c] returns a random member from the set at the key [k] on connection [c], as per the [SRANDMEMBER] redis keyword. *)
val srandmember : 'a Connection.t -> string -> string option

val psrandmember : 'a Connection.t -> string -> ('a -> string option -> 'a) -> unit

(** {3:sorted_sets_cmd Commands operating on sorted sets (zsets)} *)

(** [zadd k s m c] adds member [m] with score [s] to the sorted set at key [k] on connection [c], as per the [ZADD] redis keyword.
    @return [true] if a new element was added, [false] otherwise.
*)
val zadd : 'a Connection.t -> string -> float -> string -> bool

val pzadd : 'a Connection.t -> string -> float -> string -> ('a -> bool -> 'a) -> unit

(** [zrem k m c] removed member [m] from the sorted set at key [k] on connection [c], as per the [ZREM] redis keyword.
    @return [false] if the member was not part of the set, [true] otherwise.
*)
val zrem : 'a Connection.t -> string -> string -> bool

val pzrem : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [zincrby k i m c] increment member [m] of set at key [k] by increment [i] on connection [c], as per the [ZINCRBY] redis keyword. *)
val zincrby : 'a Connection.t -> string -> float -> string -> float

val pzincrby : 'a Connection.t -> string -> float -> string -> ('a -> float -> 'a) -> unit

(** [zrank k m c] returns the rank of member [m] at key [k] on connection [c], as per the [ZRANK] redis keyword. *)
val zrank : 'a Connection.t -> string -> string -> int option

val pzrank : 'a Connection.t -> string -> string -> ('a -> int option -> 'a) -> unit

(** [zrevrank k m c] returns the reverse rank of member [m] at key [k] on connection [c], as per the [ZREVRANK] redis keyword. *)
val zrevrank : 'a Connection.t -> string -> string -> int option

val pzrevrank : 'a Connection.t -> string -> string -> ('a -> int option -> 'a) -> unit

(** [zrange k s e c] returns an in-order list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZRANGE] redis keyword. *)
val zrange : 'a Connection.t -> string -> int -> int -> string list

val pzrange : 'a Connection.t -> string -> int -> int -> ('a -> string list -> 'a) -> unit

(** [zrange_withscores k s e c], returns an in-order list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c]. This is exactly like the {!zrange} function except it also gives the score for each item.
    @return a list of [({!string}, float)] for the specified range.
*)
val zrange_with_scores : 'a Connection.t -> string -> int -> int -> (string * float) list

val pzrange_with_scores : 'a Connection.t -> string -> int -> int -> ('a -> (string * float) list -> 'a) -> unit

(** [zrevrange k s e c] returns a {i reversed ordered} list of members of the sorted set at key [k] between the start index [s] and the end index [e], inclusively, on connection [c], as per the [ZREVRANGE] redis keyword. *)
val zrevrange : 'a Connection.t -> string -> int -> int -> string list

val pzrevrange : 'a Connection.t -> string -> int -> int -> ('a -> string list -> 'a) -> unit

(** [zrevrange_withscores k s e c], returns a {i reversed ordered} list of members of the set at sorted key [k] between the start index [s] and the end index [e], inclusively, on connection [c]. This is exactly like the {!zrevrange} function except it also gives the score for each item.
    @return a list of [({string}, float)] for the specified range.
*)
val zrevrange_with_scores : 'a Connection.t -> string -> int -> int -> (string * float) list 

val pzrevrange_with_scores : 'a Connection.t -> string -> int -> int -> ('a -> (string * float) list -> 'a) -> unit

(** [zrangebyscore k min max limit c] returns a list of all the members in sorted set at the key [k] with scores between [min] and [max], inclusively, on connection [c], as per the [ZRANGEBYSCORE] redis keyword.
    @param limit Pass in [`Limit(offset, limit)] to limit the number of returned values by [limit] offset by [offset].
*)
val zrangebyscore :
  'a Connection.t -> 
  ?limit:limit ->
  string ->
  float ->
  float -> string list

val pzrangebyscore :
  'a Connection.t -> 
  ?limit:limit ->
  string ->
  float ->
  float -> 
  ('a -> string list -> 'a) -> unit

(** [zcard k c] returns the number of members in the sorted set at the key [k] on connection [c], as per the [ZCARD] redis keyword. *)
val zcard : 'a Connection.t -> string -> int

val pzcard : 'a Connection.t -> string -> ('a -> int -> 'a) -> unit

(** [zscore k e c] returns the score of the element [e] of the sorted set at the key [k] on connection [c], as per the [ZSCORE] redis keyword. *)
val zscore : 'a Connection.t -> string -> string -> float option

val pzscore : 'a Connection.t -> string -> string -> ('a -> float option -> 'a) -> unit

(** [zremrangebyrank k s e c] removes all the members of key [k] on connection [c] in the ranks starting at [s] and ending at [e], as per the [ZREMRANGEBYRANK] redis keyword.
    @return the number of deleted members.
*)
val zremrangebyrank : 'a Connection.t -> string -> int -> int -> int

val pzremrangebyrank : 'a Connection.t -> string -> int -> int -> ('a -> int -> 'a) -> unit

(** [zremrangebyscore k min max] removes all the memebers of the sorted set at the key [k] with scores between [min] and [max] on connection [c], as per the [ZREMRANGEBYSCORE] redis keyword.
    @return the number of elements removed.
*)
val zremrangebyscore : 'a Connection.t -> string -> float -> float -> int

val pzremrangebyscore : 'a Connection.t -> string -> float -> float -> ('a -> int -> 'a) -> unit

(** [zunionstore d kl aggregate c] stores the union of all the members in the sorted sets at [kl] in destination key [d] by aggregating by [aggregate] on connection [c], as per the [ZUNIONSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zunionstore : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> int

val pzunionstore : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> ('a -> int -> 'a) -> unit

(** [zunionstore_with_weights d kl wl aggregate c] stores the union of all the members in the sorted sets at [kl] in destination key [d] by first multiplying the scores in each key by wl and then by aggregating by [aggregate] on connection [c], as per the [ZUNIONSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zunionstore_with_weights : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> float list -> int

val pzunionstore_with_weights : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> float list -> ('a -> int -> 'a) -> unit

(** [zinterstore d kl aggregate c] stores the intersection of all the members in the sorted sets at [kl] in destination key [d] by aggregating by [aggregate] on connection [c], as per the [ZINTERSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zinterstore : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> int

val pzinterstore : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> ('a -> int -> 'a) -> unit

(** [zinterstore_with_weights d kl wl aggregate c] stores the intersection of all the members in the sorted sets at [kl] in destination key [d] by first multiplying the scores in each key by wl and then by aggregating by [aggregate] on connection [c], as per the [ZINTERSTORE] redis keyword.
    @param aggregate way to aggregate scores across all the same members of different keys. Either [`Sum] (the default), [`Min] or [`Max].
    @return The number of members now in the destination key.
*)
val zinterstore_with_weights : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> float list -> int

val pzinterstore_with_weights : 'a Connection.t -> ?aggregate:aggregate -> string -> string list -> float list -> ('a -> int -> 'a) -> unit

(** {3:hash_cmd Commands operating on hashes} *)

(** [hset k f v c] sets field [f] to value [v] at key [k] on connection [c], as per the [HSET] redis keyword.
    @return [true] if the field does not already exist in the hash
*)
val hset : 'a Connection.t -> string -> string -> string -> bool

val phset : 'a Connection.t -> string -> string -> string -> ('a -> bool -> 'a) -> unit

(** [hsetnx k f v c] sets field [f] to value [v] at key [k] on connection [c], as per the [HSETNX] redis keyword.
    @return [false] if the field does not already exist in the hash and no operation was performed, [true] otherwise.
*)
val hsetnx : 'a Connection.t -> string -> string -> string -> bool

val phsetnx : 'a Connection.t -> string -> string -> string -> ('a -> bool -> 'a) -> unit

(** [hdel k f c] deletes field [f] from key [k] on connection [c], as per the [HDEL] redis keyword.
    @return [true] if the field existed and was deleted, [false] otherwise
*)
val hdel : 'a Connection.t -> string -> string -> bool

val phdel : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [hget k f c] retrieves the string stored for field [f] at key [k] on connection [c], as per the [HGET] redis keyword.
    @return {!string} [Nil] if the field or the key cannot be found.
*)
val hget : 'a Connection.t -> string -> string -> string option

val phget : 'a Connection.t -> string -> string -> ('a -> string option -> 'a) -> unit

(** [hmget k fl c] retrieves the list of fields [fl] off key [k] on connection [c], as per the [HMGET] redis keyword.
*)
val hmget : 'a Connection.t -> string -> string list -> string option list

val phmget : 'a Connection.t -> string -> string list -> ('a -> string option list -> 'a) -> unit

(** [hmset k fv c] sets the list of field-value pairs [fv] on connection [c], as per the [HMSET] redis keyword.
*)
val hmset : 'a Connection.t -> string -> (string * string) list -> unit

val phmset : 'a Connection.t -> string -> (string * string) list -> unit

(** [hincrby k f v c] increments the field [f] on key [k] by value [v] on connection [c], as per the [HINCRBY] redis keyword.
*)
val hincrby : 'a Connection.t -> string -> string -> int -> int64

val phincrby : 'a Connection.t -> string -> string -> int -> ('a -> int64 -> 'a) -> unit

(** [hexists k f c] returns [true] if field [f] exists at key [k] on connection [c], [false] otherwise, as per the [HEXISTS] redis keyword.
*)
val hexists : 'a Connection.t -> string -> string -> bool

val phexists : 'a Connection.t -> string -> string -> ('a -> bool -> 'a) -> unit

(** [hlen k c] returns the number of fields at key [k] on connection [c], as per the [HLEN] redis keyword.
*)
val hlen : 'a Connection.t -> string -> int

val phlen : 'a Connection.t -> string -> ('a -> int -> 'a) -> unit

(** [hkeys k c] returns a list of all the fields at key [k] on connection [c], as per the [HKEYS] redis keyword.
*)
val hkeys : 'a Connection.t -> string -> string list

val phkeys : 'a Connection.t -> string -> ('a -> string list -> 'a) -> unit

(** [hvals k c] returns a list of all the values tied to fields at key [k] on connection [c], as per the [HVALS] redis keyword.
*)
val hvals : 'a Connection.t -> string -> string list

val phvals : 'a Connection.t -> string -> ('a -> string list -> 'a) -> unit

(** [hgetall k c] returns the pairs of field/values stored at key [k] on connection [c], as per the [HGETALL] redis keyword.
*)
val hgetall : 'a Connection.t -> string -> (string * string) list

val phgetall : 'a Connection.t -> string -> ('a -> (string * string) list -> 'a) -> unit

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
  'a Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?get:redis_sort_pattern ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string -> string list

val psort :
  'a Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?get:redis_sort_pattern ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string -> 
  ('a -> string list -> 'a) -> unit

(** [sort_get_many k gt pattern limit order alpha c] sorts a list, set or sorted set at key [k] on connection [c], as per the [SORT] redis keyword. This function is a way to use the [GET] keyword multiple times as per the redis spec and collate them into one list while not dealing with wrapping and unwrapping values from lists in the simplest case with {!sort}.
    @return a list of lists of values "gotten" by the patterns in the list [gt]
    @param pattern a {!redis_sort_pattern}, [NoPattern] by default.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort_get_many :
  'a Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string ->
  string list -> string list list

val psort_get_many :
  'a Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string ->
  string list -> 
  ('a -> string list list -> 'a) -> unit

(** [sort_and_store k gt d pattern limit order alpha c] sorts a list, set or sorted set at key [k] on connection [c], and places the results at key [d], as per the [SORT] redis keyword. This function is a way to use the [STORE] keyword with either one or multiple [GET]s times as per the redis spec.
    @return the length of the destination list at key [d]
    @param gt a list of keys to store in the destination list, can be a list with one item.
    @param pattern a {!redis_sort_pattern}, [NoPattern] by default.
    @param limit either [`Unlimited] to return unlimited values (the default) or [`Limit(limit, offset)] to limit to [limit] results offset by [offset].
    @param order either [`Asc] to sort ascending (the default) or [`Desc] for descending.
    @param alpha either [`NonAlpha] to sort numerically (the default) or [`Alpha] to sort alphanumerically.
*)
val sort_and_store :
  'a Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string ->
  string list ->
  string -> int

val psort_and_store :
  'a Connection.t -> 
  ?pattern:redis_sort_pattern ->
  ?limit:limit ->
  ?order:sort_order ->
  ?alpha:sort_alpha -> 
  string ->
  string list ->
  string -> 
  ('a -> int -> 'a) -> unit

(** {3:persistence_cmd Persistence control commands} *)

(** [save c] synchronously save the DB to disk on connection [c], as per the [SAVE] redis keyword. *)
val save : 'a Connection.t -> unit

val psave : 'a Connection.t -> unit

(** [bgsave c] asynchronously save the DB to disk on connection [c], as per the [BGSAVE] redis keyword. *)
val bgsave : 'a Connection.t -> unit

val pbgsave : 'a Connection.t -> unit

(** [lastsave c] returns the UNIX time of the last save on connection [c], as per the [LASTSAVE] redis keyword.
    @return a float, because the unix time is bigger than the int size in ocaml on 32 bit architectures.
*)
val lastsave : 'a Connection.t -> int64

val plastsave : 'a Connection.t -> ('a -> int64 -> 'a) -> unit

(** [shutdown c] write to disk and then shuts down the server on connection [c], as per the [SHUTDOWN] redis keyword. *)
val shutdown : 'a Connection.t -> unit

(** [bgrewriteaof c] rewrite the append only file in the background on connection [c], as per the [BGREWRITEAOF] redis keyword. *)
val bgrewriteaof : 'a Connection.t -> unit

val pbgrewriteaof : 'a Connection.t -> unit

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
val info : 'a Connection.t -> Info.t

(** [slaveof a p c] makes the redis server a slave of another redis server at host [a] and port [o] on connection [c], as per the [SLAVEOF] redis keyword. *)
val slaveof : 'a Connection.t -> string -> int -> unit

  
