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

let test_srem () =
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

let test_spop () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.spop "rory" connection)
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

let test_sismember () =
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
        assert ( [Redis.String("cool")] = Redis.smembers "rory" connection )
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

let test_sinter () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "cool" connection);
        assert ( [Redis.String("cool")] = Redis.sinter ["rory"; "tim"] connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SADD tim 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SINTER rory tim");
            Script.WriteThisLine("*1");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool")
        ]
        test_func;;

let test_sinterstore () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "cool" connection);
        assert (1 == Redis.sinterstore "bob" ["rory"; "tim"] connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SADD tim 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SINTERSTORE bob rory tim");
            Script.WriteThisLine(":1")
        ]
        test_func;;

let test_sunion () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "cool" connection);
        assert ([Redis.String("cool")] = Redis.sunion ["rory"; "tim"] connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SADD tim 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SUNION rory tim");
            Script.WriteThisLine("*1");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool")
        ]
        test_func;;

let test_sunionstore () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "not so cool" connection);
        assert ( 2 = Redis.sunionstore "bob" ["rory"; "tim"] connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SADD tim 11");
            Script.ReadThisLine("not so cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SUNIONSTORE bob rory tim");
            Script.WriteThisLine(":2")
        ]
        test_func;;

let test_sdiff () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( [Redis.String("cool")] = Redis.sdiff "rory" ["tim"] connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SDIFF rory tim");
            Script.WriteThisLine("*1");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool")
        ]
        test_func;;

let test_sdiffstore () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( 1 = Redis.sdiffstore "bob" "rory" ["tim"] connection )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SDIFFSTORE bob rory tim");
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_srandmember () =
    let test_func connection =
        ignore (Redis.sadd "cool" "rory" connection);
        assert ( Redis.String("rory") = (Redis.srandmember "cool" connection))
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SADD cool 4");
            Script.ReadThisLine("rory");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SRANDMEMBER cool");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory");
        ]
        test_func;;
