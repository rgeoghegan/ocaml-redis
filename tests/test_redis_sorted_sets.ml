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
