Redis client library for Ocaml
==============================

ocaml-redis implements the client spec of the [Redis key-value store](http://code.google.com/p/redis/). It currently only implements the commands operating on string values listed on the [Redis Command Reference](http://code.google.com/p/redis/wiki/CommandReference) for *Redis 1.0*.

Testing
-------

To run all the unit tests, execute:

    rake test

To run a simple smoke test on a redis server *you don't mind clobbering* running on your localhost, execute:

    rake smoke_test
