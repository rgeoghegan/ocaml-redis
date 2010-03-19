(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on sorted sets (zsets, Redis version >= 1.1)" *)

let test_zadd () =
    let test_func connection =
        assert (Redis.zadd "rory" 42.0 "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
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
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "rory" 13.0 "strong" connection);
        assert(
            [ Redis.String("strong"); Redis.String("cool")]
            = Redis.zrange "rory" 0 1 connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZADD rory 13.000000 6");
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

let test_zrevrange () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "rory" 13.0 "strong" connection);
        assert(
            [ Redis.String("cool"); Redis.String("strong")]
            = Redis.zrevrange "rory" 0 1 connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZADD rory 13.000000 6");
            Script.ReadThisLine("strong");
            Script.WriteThisLine(":1");

            Script.ReadThisLine("ZREVRANGE rory 0 1");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.WriteThisLine("$6");
            Script.WriteThisLine("strong");
        ]
        test_func;;

let test_zrangebyscore () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "rory" 13.0 "strong" connection);
        assert(
            [ Redis.String("cool"); Redis.String("strong")]
            = Redis.zrangebyscore "rory" 0.0 100.0 connection);
        assert(
            [ Redis.String("strong")]
            = Redis.zrangebyscore "rory" 0.0 100.0 ~limit:(`Limit(1,1)) connection
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZADD rory 13.000000 6");
            Script.ReadThisLine("strong");
            Script.WriteThisLine(":1");

            Script.ReadThisLine("ZRANGEBYSCORE rory 0.000000 100.000000");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.WriteThisLine("$6");
            Script.WriteThisLine("strong");

            Script.ReadThisLine("ZRANGEBYSCORE rory 0.000000 100.000000 LIMIT 1 1");
            Script.WriteThisLine("*1");
            Script.WriteThisLine("$6");
            Script.WriteThisLine("strong");
        ]
        test_func;;

let test_zincrby () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (43.0 = Redis.zincrby "rory" 1.0 "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZINCRBY rory 1.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("$2");
            Script.WriteThisLine("43")
        ]
        test_func;;

let test_zcard () =
    let test_func connection =
        assert (0 = Redis.zcard "rory" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZCARD rory");
            Script.WriteThisLine(":0")
        ]
        test_func;;

let test_zscore () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (42.0 = Redis.zscore "rory" "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZSCORE rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("$2");
            Script.WriteThisLine("42")
        ]
        test_func;;

let test_zremrangebyscore () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (1 = Redis.zremrangebyscore "rory" 30.0 50.0 connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("ZADD rory 42.000000 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("ZREMRANGEBYSCORE rory 30.000000 50.000000");
            Script.WriteThisLine(":1");
        ]
        test_func;;
        
    
