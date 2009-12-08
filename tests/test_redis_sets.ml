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

let test_rem () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert (Redis_util.String("cool") = Redis.spop "rory" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SPOP rory");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
        ]
        test_func;;

let test_smove () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "even cooler" connection);
        assert ( Redis.smove "tim" "rory" "even cooler" connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SADD tim 11");
            Script.ReadThisLine("even cooler");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SMOVE tim rory 11");
            Script.ReadThisLine("even cooler");
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_scard () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "rory" "even cooler" connection);
        assert ( 2 = Redis.scard "rory" connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SADD rory 11");
            Script.ReadThisLine("even cooler");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SCARD rory");
            Script.WriteThisLine(":2");
        ]
        test_func;;

let test_scard () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( Redis.sismember "rory" "cool" connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SISMEMBER rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_smembers () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( [Redis_util.String("cool")] = Redis.smembers "rory" connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SMEMBERS rory");
            Script.WriteThisLine("*1");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool")
        ]
        test_func;;
