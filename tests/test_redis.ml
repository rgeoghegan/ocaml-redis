(* Individual commands *)
let test_ping () =
    let test_func connection =
        assert (
            (Redis.ping connection) = true
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("PING");
            Script.WriteThisLine("+PONG")
        ]
        test_func;;

let test_exists () =
    let test_func connection = begin
        assert (
            (Redis.exists "real_key" connection) = true
        );
        assert (
            (Redis.exists "fake_key" connection) = false
        )
    end in
    Script.use_test_script
        [
            Script.ReadThisLine("EXISTS real_key");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("EXISTS fake_key");
            Script.WriteThisLine(":0")
        ]
        test_func;;

let test_flushdb () =
    let test_func conn =
        assert (
            true = Redis.flushdb conn
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("FLUSHDB");
            Script.WriteThisLine("+OK")
        ]
        test_func;;
