(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Miscelanious commands *)

let test_ping () =
    let test_func connection =
        assert (
            Redis.ping connection
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("PING");
            Script.WriteThisLine("+PONG")
        ]
        test_func;;

let test_quit () =
    let test_func connection =
        Redis.quit connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("QUIT");
        ]
        test_func;;

let test_auth () =
    let test_func connection =
        Redis.auth "qwerty" connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("AUTH qwerty");
            Script.WriteThisLine("+OK")
        ]
        test_func;;
