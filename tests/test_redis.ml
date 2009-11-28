(*
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

 let test_set () =
    let test_func connection =
        Redis.set "key" "aaa" connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SET key 3");
            Script.ReadThisLine("aaa");
            Script.WriteThisLine("+OK")
        ]
        test_func;;

 let test_get () =
    let test_func connection = 
        assert (
            (Redis.get "key" connection) = Redis.Data("aaa")
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("GET key");
            Script.WriteThisLine("$3");
            Script.WriteThisLine("aaa")
        ]
        test_func;;

 let test_getset () =
    let test_func connection =
        assert (
            (Redis.getset "key" "now" connection) = Redis.Data("previous")
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("GETSET key 3");
            Script.ReadThisLine("now");
            Script.WriteThisLine("$8");
            Script.WriteThisLine("previous")
        ]
        test_func;;

 let test_mget () =
    let test_func conn =
        assert (
            (Redis.mget ["rory"; "tim"] conn) = [Redis.Data("cool"); Redis.Data("not cool")]
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("MGET rory tim");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.WriteThisLine("$8");
            Script.WriteThisLine("not cool")
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
*)
