(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Simple smoke test to be run on local server. *)

let smoke_test_with_quit conn = begin
    Redis.auth "qwerty" conn;
    Redis.flushall conn; 

    assert ( false = Redis.exists "rory" conn);
    Redis.set "rory" "cool" conn;
    assert ( Redis_util.String("cool") = Redis.get "rory" conn);
    assert ( Redis_util.String("cool") = Redis.getset "rory" "not cool" conn);
    assert ( [Redis_util.String("not cool"); Redis_util.Nil] = Redis.mget ["rory"; "tim"] conn);
    assert ( false = Redis.setnx "rory" "uncool" conn);
    assert ( Redis_util.String("not cool") = Redis.get "rory" conn);

    Redis.mset [("rory", "cool"); ("tim", "not cool")] conn;
    assert (not (Redis.msetnx [("rory", "not cool"); ("tim", "cool")] conn));

    assert ( 1 = Redis.incr "rory" conn);
    assert ( 5 = Redis.incrby "rory" 4 conn);
    assert ( 4 = Redis.decr "rory" conn);
    assert ( 2 = Redis.decrby "rory" 2 conn);

    assert ( 2 = Redis.del ["rory"; "tim"] conn);

    Redis.set "rory" "cool" conn;
    assert ( Redis_util.RedisNil = (Redis.value_type "tim" conn));
    assert ( Redis_util.RedisString = (Redis.value_type "rory" conn));

    assert ( ["rory"] = Redis.keys "*" conn);
    assert ( "rory" = Redis.randomkey conn);
    
    Redis.rename "rory" "tim" conn;
    assert ( "tim" = Redis.randomkey conn);

    Redis.set "rory" "more cool" conn;
    assert ( false = Redis.renamenx "rory" "tim" conn);

    assert ( 2 == Redis.dbsize conn );

    assert ( Redis.expire "rory" 10 conn );
    assert ( 10 >= Redis.ttl "rory" conn );

    (* List operations *)
    ignore (Redis.del ["rory"] conn);
    Redis.rpush "rory" "cool" conn;
    Redis.lpush "rory" "even cooler" conn;
    assert ( 2 == (Redis.llen "rory" conn));
    assert ( [Redis_util.String("even cooler"); Redis_util.String("cool")] = (Redis.lrange "rory" 0 1 conn));

    Redis.ltrim "rory" 0 0 conn;
    assert ( (Redis_util.string_of_bulk_data (Redis.lindex "rory" 0 conn)) = "even cooler");
    Redis.lset "rory" 0 "just cool" conn;
    Redis.rpush "rory" "cool" conn;
    assert ( 1 = Redis.lrem "rory" 0 "cool" conn);

    Redis.rpush "rory" "cool" conn;
    Redis.rpush "rory" "even cooler" conn;

    assert ( (Redis_util.string_of_bulk_data (Redis.lpop "rory" conn)) = "just cool");
    assert ( (Redis_util.string_of_bulk_data (Redis.rpop "rory" conn)) = "even cooler");

    Redis.rpush "cool" "rory" conn;
    Redis.rpush "cool" "tim" conn;
    assert ( (Redis_util.string_of_bulk_data (Redis.rpoplpush "cool" "not_cool" conn)) = "tim");

    (* Set operations *)
    ignore (Redis.del_one "tim" conn);
    assert ( Redis.sadd "tim" "not cool" conn);
    assert ( Redis.sadd "tim" "smells" conn);

    assert ( Redis.srem "tim" "smells" conn);
    
    assert ( "not cool" = Redis_util.string_of_bulk_data (Redis.spop "tim" conn) );

    ignore ( Redis.del_one "rory" conn);
    assert ( Redis.sadd "rory" "cool" conn);
    assert ( Redis.sadd "tim" "even cooler" conn);
    assert ( Redis.smove "tim" "rory" "even cooler" conn );
    
    assert ( 2 = Redis.scard "rory" conn );

    assert ( Redis.sismember "rory" "cool" conn );

    ignore ( Redis.srem "rory" "cool" conn );
    assert ( "even cooler" = Redis_util.string_of_bulk_data (List.hd (Redis.smembers "rory" conn)) );

    ignore (Redis.sadd "tim" "even cooler" conn);
    assert ( "even cooler" = Redis_util.string_of_bulk_data (List.hd (Redis.sinter ["rory"; "tim"] conn)) );
    
    assert ( 1 = Redis.sinterstore "bob" ["rory"; "tim"] conn );

    assert ( "even cooler" = Redis_util.string_of_bulk_data (List.hd (Redis.sunion ["rory"; "tim"] conn)) );
    assert ( 1 = Redis.sunionstore "bob" ["rory"; "tim"] conn );
    ignore ( Redis.srem "tim" "even cooler" conn );
    assert ( "even cooler" = Redis_util.string_of_bulk_data (List.hd (Redis.sdiff ["rory"; "tim"] conn)) );
    assert ( 1 = Redis.sdiffstore "bob" ["rory"; "tim"] conn);

    ignore (Redis.del_one "rory" conn);
    ignore (Redis.del_one "tim" conn);
    ignore (Redis.sadd "rory" "cool" conn);
    assert ( "cool" = Redis_util.string_of_bulk_data (Redis.srandmember "rory" conn));
    assert (Redis_util.Nil = Redis.srandmember "non_existent_key" conn);

    (* Multiple databases *)
    Redis.select 1 conn;

    Redis.select 0 conn;
    assert ( Redis.move "rory" 1 conn );

    Redis.lpush "rory" "1" conn;
    Redis.lpush "rory" "2" conn;
    Redis.lpush "rory" "11" conn;

    (* Sorted sets *)
    assert (Redis.zadd "coolest" 42.0 "rory" conn);
    assert (Redis.zrem "coolest" "rory" conn);

    ignore (Redis.zadd "coolest" 1.0 "rory" conn);
    ignore (Redis.zadd "coolest" 99.0 "tim" conn);
    assert (
        ["rory"; "tim"]
        = List.map
            Redis_util.string_of_bulk_data
            (Redis.zrange "coolest" 0 1 conn));

    assert (
        ["tim"; "rory"]
        = List.map
            Redis_util.string_of_bulk_data
            (Redis.zrevrange "coolest" 0 1 conn));

    assert (
        "rory"
        = Redis_util.string_of_bulk_data
            (List.hd
                (Redis.zrangebyscore
                    "coolest" 0.0 100.0 ~limit:(`Limit(0,1)) conn)));

    assert (
        2.0
        = Redis.zincrby "coolest" 1.0 "rory" conn);

    assert (2 = Redis.zcard "coolest" conn);

    assert (2.0 = Redis.zscore "coolest" "rory" conn);

    assert (1 = Redis.zremrangebyscore "coolest" 80.0 120.0 conn);

    (* Sort *)
    assert ( "2" = Redis_util.string_of_bulk_data (List.hd (
        Redis.sort "rory" ~alpha:`Alpha ~order:`Desc conn
    )));
    
    (* Remote server control commands *)
    assert ( "master" = Redis.Info.get
        (Redis.info conn)
        "role"
    );
    
    (* Persistence *)
    Redis.save conn;
    Redis.bgsave conn;

    assert ( 0.0 < Redis.lastsave conn);
    Redis.bgrewriteaof conn;

    Redis.flushall conn;
    Redis.quit conn;
    print_endline "Smoke test passed"
end;;

let smoke_test_with_shutdown conn = begin
    Redis.auth "qwerty" conn;
   Redis.shutdown conn
end

let _ =
    let default_connection () = Redis.create_connection ()
    in
    begin
        smoke_test_with_quit (default_connection ());
        smoke_test_with_shutdown (default_connection ())
    end;;
