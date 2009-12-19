Redis client library for Ocaml
==============================

ocaml-redis implements the client spec of the [Redis key-value store](http://code.google.com/p/redis/).

It currently only implements the commands listed in the following subsection of the [Redis Command Reference](http://code.google.com/p/redis/wiki/CommandReference) for *Redis 1.0*:

* Commands operating on string values
* Commands operating on the key space
* Commands operating on lists
* Multiple databases handling commands
* Sorting
* Persistence control commands
* Connection handling

Testing
-------

To run all the unit tests, execute:

    rake test

To run a simple smoke test on a redis server *you do not mind completely wiping* running on your localhost, execute:

    rake smoke_test

Todo
----

The following sections or part of sections are currently unimplemented:

* Remote server control commands
* Commands operating on string values
 * MSET
 * MSETNX
* Commands operating on sets
 * SINTERSTORE
* Commands operating on lists
 * RPOPLPUSH
* Commands operating on sorted sets
