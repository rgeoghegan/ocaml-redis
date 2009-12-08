(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on sets" *)

let test_sadd () =
    let test_func connection =
        assert (Redis.sadd "rory" "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_rem () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert (Redis.srem "rory" "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SREM rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
        ]
        test_func;;
