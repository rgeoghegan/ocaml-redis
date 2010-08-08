Redis client library for Ocaml
==============================

ocaml-redis implements the client spec of the [Redis key-value store](http://code.google.com/p/redis/).

This branch is specifically implementing Redis 2.0, and is not compatible with Redis 1.2.5.

Example Usage
-------------

    >> let conn = Redis.create_connection ()
    in
    begin
        Redis.lpush "redis" "works" conn;
        Redis.lpush "redis" "fast" conn;
        Redis.lpush "redis" "simple" conn;
        List.map Redis.string_of_bulk_data
            (Redis.lrange "redis" 0 2 conn);
    end;;
    ["simple"; "fast"; "works"]

Building
--------

To build the library,

    rake library

should do the trick. From there, you will have to statically link build/redis.cmx, build/redis.cmo and build/redis.cmi with your code.

Testing
-------

To run all the unit tests, execute:

    rake test

To run a simple smoke test on a redis server *you do not mind completely wiping* running on your localhost, execute:

    rake smoke_test

Todo
----

See the [issue tracker](http://github.com/rgeoghegan/ocaml-redis/issues)
