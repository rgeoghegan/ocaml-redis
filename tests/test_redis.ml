(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Miscelanious commands *)

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
