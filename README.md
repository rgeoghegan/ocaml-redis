Redis client library for Ocaml
==============================

ocaml-redis implements the client spec of the [Redis key-value store](http://code.google.com/p/redis/).

It currently only implements the commands listed in the following subsection of the [Redis Command Reference](http://code.google.com/p/redis/wiki/CommandReference) for *Redis 1.0*:

* Commands operating on string values
* Commands operating on the key space
* Commands operating on lists
* Commands operating on sets (except SINTERSTORE)
* Multiple databases handling commands
* Sorting

These sections are still to be completed:

* Connection handling
* Persistence control commands
* Remote server control commands

Testing
-------

To run all the unit tests, execute:

    rake test

To run a simple smoke test on a redis server *you don't mind clobbering* running on your localhost, execute:

    rake smoke_test
