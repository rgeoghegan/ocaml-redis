let smoke_test conn = begin
    assert ( false = Redis.exists "rory" conn);
    Redis.set "rory" "cool" conn;
    assert ( Redis_util.Data("cool") = Redis.get "rory" conn);
    assert ( Redis_util.Data("cool") = Redis.getset "rory" "not cool" conn);
    assert ( [Redis_util.Data("not cool"); Redis_util.Nil] = Redis.mget ["rory"; "tim"] conn);

    (*
    Redis.setnx "rory" "uncool" conn;
    assert ( Redis_util.Data("not cool") = Redis.get "rory" conn); *)
    ignore (Redis.flushdb conn); 
    print_endline "Smoke test passed"
end;;

let _ =
    let default_connection = Redis.create_connection "127.0.0.1" 6379
    in
    smoke_test default_connection;;
