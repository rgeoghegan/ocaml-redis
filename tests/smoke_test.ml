let smoke_test conn = begin
    assert ( true = Redis.ping conn );
    assert ( false = Redis.exists "rory" conn);
    Redis.set "rory" "cool" conn;
    assert ( Redis.Data("cool") = Redis.get "rory" conn);
    assert ( Redis.Data("cool") = Redis.getset "rory" "not cool" conn);
    assert ( [Redis.Data("not cool"); Redis.Nil] = Redis.mget ["rory"; "tim"] conn);
(*
    Redis.flushdb conn;
*)
    print_endline "Smoke test passed"
end;;

let _ =
    let default_connection = Redis.create_connection "127.0.0.1" 6379
    in
    smoke_test default_connection;;
