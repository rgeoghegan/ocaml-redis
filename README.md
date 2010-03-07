Redis client library for Ocaml
==============================

ocaml-redis implements the client spec of the [Redis key-value store](http://code.google.com/p/redis/).

It currently only implements the commands listed in the following subsection of the [Redis Command Reference](http://code.google.com/p/redis/wiki/CommandReference) for Redis 1.2.4:

 * Connection handling
 * Commands operating on all the kind of values
 * Commands operating on string values
 * Commands operating on lists
 * Commands operating on sets
  * sadd
  * srem
  * spop
  * smove
  * scard
  * sismember
  * sinter
  * sinterstore
  * sunion
  * sunionstore
  * sdiff
  * sdiffstore
  * smembers
 * Commands operating on sorted sets (zsets)
  * zadd
  * zrem
  * zrange
  * zrevrange
  * zrangebyscore
 * Sorting
 * Persistence control commands
  * save
  * bgsave
  * lastsave
  * shutdown

### Missing:

 * Commands operating on sets
  * srandmember
 * Commands operating on sorted sets (zsets)
  * zincrby
  * zcard
  * zscore
  * zremrangebyscore
 * Persistence control commands
  * bgrewriteaof
 * Remote server control commands (all)

Building
--------

To build the library,

    rake library

should do the trick. From there, you will have to manually move redis.cmxa, redis.a, redis.cmi and redis_util.cmi to where ever suits you best.

Testing
-------

To run all the unit tests, execute:

    rake test

To run a simple smoke test on a redis server *you do not mind completely wiping* running on your localhost, execute:

    rake smoke_test

Todo
----

 * Implement all missing keywords from 1.2.4:
  * Commands operating on lists
   * rpoplpush
  * Commands operating on sets
   * srandmember
  * Commands operating on sorted sets (zsets)
   * zincrby
   * zcard
   * zscore
   * zremrangebyscore
  * Persistence control commands
   * bgrewriteaof
  * Remote server control commands (all)

 * Refactors:
  * Seperate sets into different code file but same module
  * Remove Big_int and use floats instead (espcially since Bit_int is not needed on 64bit machines)
  * Create type for connection
  * Create 'send_and_receive_command_safely' which checks for statuses
  * Compile smoke_test module to avoid stray .cmx file
