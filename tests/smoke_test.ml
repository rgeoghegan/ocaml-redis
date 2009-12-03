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
    ignore (Redis.renamenx "rory" "tim" conn);
    assert ( Redis_util.String("more cool") = Redis.get "rory" conn);
    assert ( Redis_util.String("cool") = Redis.get "tim" conn);

    ignore (Redis.flushdb conn); 
    print_endline "Smoke test passed"
end;;

let _ =
    let default_connection = Redis.create_connection "127.0.0.1" 6379
    in
    smoke_test default_connection;;
