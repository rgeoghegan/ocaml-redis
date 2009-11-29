let smoke_test conn = begin
    assert ( false = Redis.exists "rory" conn);
    Redis.set "rory" "cool" conn;
    assert ( Redis_util.Data("cool") = Redis.get "rory" conn);
    assert ( Redis_util.Data("cool") = Redis.getset "rory" "not cool" conn);
    assert ( [Redis_util.Data("not cool"); Redis_util.Nil] = Redis.mget ["rory"; "tim"] conn);
    assert ( false = Redis.setnx "rory" "uncool" conn);
    assert ( Redis_util.Data("not cool") = Redis.get "rory" conn);

    assert ( 1 = Redis.incr "rory" conn);
    assert ( 5 = Redis.incrby "rory" 4 conn);
    assert ( 4 = Redis.decr "rory" conn);
    (*
    assert ( 2 = Redis.decrby "rory" conn);
    *)

    ignore (Redis.flushdb conn); 
    print_endline "Smoke test passed"
end;;

let _ =
    let default_connection = Redis.create_connection "127.0.0.1" 6379
    in
    smoke_test default_connection;;
