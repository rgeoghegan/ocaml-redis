(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Simple smoke test to be run on local server. *)

let smoke_test_with_quit conn = begin
    Redis.auth "qwerty" conn;
    Redis.flushall conn; 

    assert ( false = Redis.exists "rory" conn);
    Redis.set "rory" "cool" conn;
    assert ( "cool" = Redis.string_of_bulk_data (Redis.get "rory" conn));
    assert ( "cool" = Redis.string_of_bulk_data (Redis.getset "rory" "not cool" conn));
    assert ( [Redis.String("not cool"); Redis.Nil] = Redis.mget ["rory"; "tim"] conn);
    assert ( false = Redis.setnx "rory" "uncool" conn);
    assert ( "not cool" = Redis.string_of_bulk_data (Redis.get "rory" conn));

    Redis.mset [("rory", "cool"); ("tim", "not cool")] conn;
    assert (not (Redis.msetnx [("rory", "not cool"); ("tim", "cool")] conn));

    Redis.set "rory" "0" conn;
    assert ( 1 = Redis.incr "rory" conn);
    assert ( 5 = Redis.incrby "rory" 4 conn);
    assert ( 4 = Redis.decr "rory" conn);
    assert ( 2 = Redis.decrby "rory" 2 conn);

    Redis.set "rory" "very " conn;
    assert (9 = Redis.append "rory" "cool" conn);

    assert ( 2 = Redis.del ["rory"; "tim"] conn);

    Redis.set "rory" "cool" conn;
    assert ( Redis.RedisNil = (Redis.value_type "tim" conn));
    assert ( Redis.RedisString = (Redis.value_type "rory" conn));

    assert ( ["rory"] = Redis.keys "*" conn); 
    assert ( "rory" = Redis.randomkey conn);
    Redis.rename "rory" "tim" conn;

    assert ( "tim" = Redis.randomkey conn);

    Redis.set "rory" "more cool" conn;
    assert ( false = Redis.renamenx "rory" "tim" conn);

    assert ( 2 == Redis.dbsize conn );

    assert ( Redis.expire "rory" 10 conn );
    assert ( 10 >= Redis.ttl "rory" conn );

    assert (Redis.expireat "tim" (Unix.time() +. 10.) conn);
    assert ( 10 >= Redis.ttl "tim" conn );

    (* List operations *)
    ignore (Redis.del ["rory"] conn);
    assert ( 1 == Redis.rpush "rory" "cool" conn);
    assert ( 2 == Redis.lpush "rory" "even cooler" conn);
    assert ( 2 == (Redis.llen "rory" conn));
    assert ( [Redis.String("even cooler"); Redis.String("cool")] = (Redis.lrange "rory" 0 1 conn));

    Redis.ltrim "rory" 0 0 conn;
    assert ( (Redis.string_of_bulk_data (Redis.lindex "rory" 0 conn)) = "even cooler");
    Redis.lset "rory" 0 "just cool" conn;
    ignore (Redis.rpush "rory" "cool" conn);
    assert (1 = Redis.lrem "rory" 0 "cool" conn);

    ignore (Redis.rpush "rory" "cool" conn);
    ignore (Redis.rpush "rory" "even cooler" conn);

    assert ( (Redis.string_of_bulk_data (Redis.lpop "rory" conn)) = "just cool");
    assert ( (Redis.string_of_bulk_data (Redis.rpop "rory" conn)) = "even cooler");

    ignore (Redis.rpush "cool" "rory" conn);
    ignore (Redis.rpush "cool" "tim" conn);
    assert ( (Redis.string_of_bulk_data (Redis.rpoplpush "cool" "not_cool" conn)) = "tim");

    assert ((Redis.string_of_bulk_data (Redis.blpop "cool" conn)) = "rory");
    assert ((Redis.blpop "cool" ~timeout:(`Seconds(1)) conn) = Redis.Nil);

    (* Set operations *)
    ignore (Redis.del_one "tim" conn);
    assert ( Redis.sadd "tim" "not cool" conn);
    assert ( Redis.sadd "tim" "smells" conn);

    assert ( Redis.srem "tim" "smells" conn);
    
    assert ( "not cool" = Redis.string_of_bulk_data (Redis.spop "tim" conn) );

    ignore ( Redis.del_one "rory" conn);
    assert ( Redis.sadd "rory" "cool" conn);
    assert ( Redis.sadd "tim" "even cooler" conn);
    assert ( Redis.smove "tim" "rory" "even cooler" conn );
    
    assert ( 2 = Redis.scard "rory" conn );

    assert ( Redis.sismember "rory" "cool" conn );

    ignore ( Redis.srem "rory" "cool" conn );
    assert ( "even cooler" = Redis.string_of_bulk_data (List.hd (Redis.smembers "rory" conn)) );

    ignore (Redis.sadd "tim" "even cooler" conn);
    assert ( "even cooler" = Redis.string_of_bulk_data (List.hd (Redis.sinter ["rory"; "tim"] conn)) );
    
    assert ( 1 = Redis.sinterstore "bob" ["rory"; "tim"] conn );

    assert ( "even cooler" = Redis.string_of_bulk_data (List.hd (Redis.sunion ["rory"; "tim"] conn)) );
    assert ( 1 = Redis.sunionstore "bob" ["rory"; "tim"] conn );
    ignore ( Redis.srem "tim" "even cooler" conn );
    assert ( "even cooler" = Redis.string_of_bulk_data (List.hd (Redis.sdiff "rory" ["tim"] conn)) );
    assert ( 1 = Redis.sdiffstore "bob" "rory" ["tim"] conn);

    ignore (Redis.del_one "rory" conn);
    ignore (Redis.del_one "tim" conn);
    ignore (Redis.sadd "rory" "cool" conn);
    assert ( "cool" = Redis.string_of_bulk_data (Redis.srandmember "rory" conn));
    assert (Redis.Nil = Redis.srandmember "non_existent_key" conn);

    (* Multiple databases *)
    Redis.select 1 conn;

    Redis.select 0 conn;
    assert ( Redis.move "rory" 1 conn );

    ignore (Redis.lpush "rory" "1" conn);
    ignore (Redis.lpush "rory" "2" conn);
    ignore (Redis.lpush "rory" "11" conn);

    (* Sorted sets *)
    assert (Redis.zadd "coolest" 42.0 "rory" conn);
    assert (Redis.zrem "coolest" "rory" conn);

    ignore (Redis.zadd "coolest" 1.0 "rory" conn);
    ignore (Redis.zadd "coolest" 99.0 "tim" conn);
    assert (
        ["rory"; "tim"]
        = List.map
            Redis.string_of_bulk_data
            (Redis.zrange "coolest" 0 1 conn));

    assert (
        [("rory", 1.0); ("tim", 99.0)]
        = List.map
            (fun (a,b) -> (Redis.string_of_bulk_data a, b))
            (Redis.zrange_withscores "coolest" 0 1 conn));

    assert (
        ["tim"; "rory"]
        = List.map
            Redis.string_of_bulk_data
            (Redis.zrevrange "coolest" 0 1 conn));

    assert (
        [("tim", 99.0); ("rory", 1.0)]
        = List.map
            (fun (a,b) -> (Redis.string_of_bulk_data a, b))
            (Redis.zrevrange_withscores "coolest" 0 1 conn));

    assert (
        "rory"
        = Redis.string_of_bulk_data
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
        
    assert ( "2" = Redis.string_of_bulk_data (List.hd (
        Redis.sort "rory" ~alpha:`Alpha ~order:`Desc conn
    )));

    (* This requires quite some test data to set up *)
    let fields = ["name"; "yob"]
    in
    let data = [
        ["Rory"; "1984"];
        ["Bob"; "1980"]
    ]
    in
    let add_record index record =
        let add_field name value =
            Redis.set (name ^ "_" ^ (string_of_int index)) value conn
        in
        begin
            ignore (Redis.rpush "people" (string_of_int index) conn);
            List.iter2 add_field fields record;
            index + 1
        end
    in
    ignore (List.fold_left add_record 1 data);
    assert (
        ["Bob"; "1980"] =
        List.map Redis.string_of_bulk_data
            (List.hd (Redis.sort_get_many "people" ["name_*"; "yob_*"] ~pattern:"yob_*" conn))
    );
    
    assert(2=
        Redis.sort_and_store "people" ["name_*"] "results" ~pattern:"yob_*" conn);
    
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
    Redis.quit conn
end;;

let smoke_test_with_shutdown conn = begin
    Redis.auth "qwerty" conn;
    Redis.shutdown conn
end

let _ =
    begin
        smoke_test_with_quit (Redis.create_connection ());
        smoke_test_with_shutdown (Redis.create_connection ());
        print_endline "\x1b[32mSmoke test passed\x1b[m"
    end;;
