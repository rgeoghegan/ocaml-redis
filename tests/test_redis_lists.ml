(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on lists" *)

let test_rpush () =
    let test_func connection =
        Redis.rpush "rory" "cool" connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("RPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("+OK");
        ]
        test_func;;

let test_lpush () =
    let test_func connection =
        Redis.lpush "rory" "cool" connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("+OK");
        ]
        test_func;;

let test_llen () =
    let test_func connection =
        Redis.rpush "rory" "cool" connection;
        assert ( 1 = (Redis.llen "rory" connection));
    in
    Script.use_test_script
        [
            Script.ReadThisLine("RPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("+OK");
            Script.ReadThisLine("LLEN rory");
            Script.WriteThisLine(":1");
        ]
        test_func;;
