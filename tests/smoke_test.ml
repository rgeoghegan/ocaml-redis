(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Simple smoke test to be run on local server. *)
let smoke_test conn = begin
    ignore (Redis.flushdb conn); 
    assert ( false = Redis.exists "rory" conn);
    Redis.set "rory" "cool" conn;
    assert ( Redis_util.String("cool") = Redis.get "rory" conn);
    assert ( Redis_util.String("cool") = Redis.getset "rory" "not cool" conn);
    assert ( [Redis_util.String("not cool"); Redis_util.None] = Redis.mget ["rory"; "tim"] conn);
    assert ( false = Redis.setnx "rory" "uncool" conn);
    assert ( Redis_util.String("not cool") = Redis.get "rory" conn);

    assert ( 1 = Redis.incr "rory" conn);
    assert ( 5 = Redis.incrby "rory" 4 conn);
    assert ( 4 = Redis.decr "rory" conn);
    assert ( 2 = Redis.decrby "rory" 2 conn);

    assert ( 1 = Redis.del ["rory"] conn);

    Redis.set "rory" "cool" conn;
    assert ( Redis_util.None = (Redis.value_type "tim" conn));
    assert ( Redis_util.String("") = (Redis.value_type "rory" conn));

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

    (* Set operations *)
    ignore (Redis.del_one "tim" conn);
    assert ( Redis.sadd "tim" "not cool" conn);
    assert ( Redis.sadd "tim" "smells" conn);

    assert ( Redis.srem "tim" "smells" conn);
    
    assert ( "not cool" = Redis_util.string_of_bulk_data (Redis.spop "tim" conn) );

    ignore (Redis.flushdb conn); 
    print_endline "Smoke test passed"
end;;

let _ =
    let default_connection = Redis.create_connection "127.0.0.1" 6379
    in
    smoke_test default_connection;;
