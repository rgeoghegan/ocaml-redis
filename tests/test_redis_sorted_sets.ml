(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on sorted sets (zsets, Redis version >= 1.1)" *)

let test_zadd () =
    let test_func connection =
        assert (Redis.zadd "rory" 42 "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_zrem () =
    let test_func connection =
        assert (not (Redis.zrem "rory" "cool" connection))
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZREM rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":0")
        ]
        test_func;;

let test_zrange () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42 "cool" connection);
        ignore (Redis.zadd "rory" 13 "strong" connection);
        assert(
            [ Redis_util.String("strong"); Redis_util.String("cool")]
            = Redis.zrange "rory" 0 1 connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZADD rory 13 6");
            Script.ReadThisLine("strong");
            Script.WriteThisLine(":1");

            Script.ReadThisLine("ZRANGE rory 0 1");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$6");
            Script.WriteThisLine("strong");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
        ]
        test_func;;
