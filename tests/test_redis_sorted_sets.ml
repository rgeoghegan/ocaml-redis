(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on sorted sets (zsets, Redis version >= 1.1)" *)

open Script;;

let test_zadd () =
    let test_func connection =
        assert (Redis.zadd "rory" 42.0 "cool" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")])
        test_func;;

let test_zrem () =
    let test_func connection =
        assert (not (Redis.zrem "rory" "cool" connection))
    in
    use_test_script
        [
            ReadThisLine("ZREM rory 4");
            ReadThisLine("cool");
            WriteThisLine(":0")
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
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "rory"; "13.000000"; "strong"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZRANGE rory 0 1");
            WriteThisLine("*2");
            WriteThisLine("$6");
            WriteThisLine("strong");
            WriteThisLine("$4");
            WriteThisLine("cool");
        ])
        test_func;;

let test_zrange_withscores () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "rory" 13.0 "strong" connection);
        assert(
            [ (Redis.String("strong"), 13.0); (Redis.String("cool"), 42.0)]
            = Redis.zrange_withscores "rory" 0 1 connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "rory"; "13.000000"; "strong"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZRANGE rory 0 1 WITHSCORES");
            WriteThisLine("*4");
            WriteThisLine("$6");
            WriteThisLine("strong");
            WriteThisLine("$2");
            WriteThisLine("13");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$2");
            WriteThisLine("42");
        ])
        test_func;;

let test_zrevrange () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "rory" 13.0 "strong" connection);
        assert(
            [ Redis.String("cool"); Redis.String("strong")]
            = Redis.zrevrange "rory" 0 1 connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "rory"; "13.000000"; "strong"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZREVRANGE rory 0 1");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$6");
            WriteThisLine("strong");
        ])
        test_func;;

let test_zrevrange_withscores () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "rory" 13.0 "strong" connection);
        assert(
            [(Redis.String("cool"), 42.0); (Redis.String("strong"), 13.0)]
            = Redis.zrevrange_withscores "rory" 0 1 connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "rory"; "13.000000"; "strong"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZREVRANGE rory 0 1 WITHSCORES");
            WriteThisLine("*4");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$2");
            WriteThisLine("42");
            WriteThisLine("$6");
            WriteThisLine("strong");
            WriteThisLine("$2");
            WriteThisLine("13");
        ])
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
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "rory"; "13.000000"; "strong"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZRANGEBYSCORE rory 0.000000 100.000000");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$6");
            WriteThisLine("strong");

            ReadThisLine("ZRANGEBYSCORE rory 0.000000 100.000000 LIMIT 1 1");
            WriteThisLine("*1");
            WriteThisLine("$6");
            WriteThisLine("strong");
        ])
        test_func;;

let test_zincrby () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (43.0 = Redis.zincrby "rory" 1.0 "cool" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZINCRBY rory 1.000000 4");
            ReadThisLine("cool");
            WriteThisLine("$2");
            WriteThisLine("43")
        ])
        test_func;;

let test_zrank () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (Redis.Rank(0) = Redis.zrank "rory" "cool" connection);
        assert (Redis.NilRank = Redis.zrank "rory" "boring" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZRANK rory 4");
            ReadThisLine("cool");
            WriteThisLine(":0");
            ReadThisLine("ZRANK rory 6");
            ReadThisLine("boring");
            WriteThisLine("$-1")
        ])
        test_func;;

let test_zrevrank () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (Redis.Rank(0) = Redis.zrevrank "rory" "cool" connection);
        assert (Redis.NilRank = Redis.zrevrank "rory" "boring" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZREVRANK rory 4");
            ReadThisLine("cool");
            WriteThisLine(":0");
            ReadThisLine("ZREVRANK rory 6");
            ReadThisLine("boring");
            WriteThisLine("$-1")
        ])
        test_func;;

let test_zcard () =
    let test_func connection =
        assert (0 = Redis.zcard "rory" connection)
    in
    use_test_script
        [
            ReadThisLine("ZCARD rory");
            WriteThisLine(":0")
        ]
        test_func;;

let test_zscore () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (42.0 = Redis.zscore "rory" "cool" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZSCORE rory 4");
            ReadThisLine("cool");
            WriteThisLine("$2");
            WriteThisLine("42")
        ])
        test_func;;

let test_zremrangebyrank () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (1 = Redis.zremrangebyrank "rory" 0 0 connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZREMRANGEBYRANK rory 0 0");
            WriteThisLine(":1")
        ])
        test_func;;

let test_zremrangebyscore () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        assert (1 = Redis.zremrangebyscore "rory" 30.0 50.0 connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZREMRANGEBYSCORE rory 30.000000 50.000000");
            WriteThisLine(":1");
        ])
        test_func;;

let test_zunionstore () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "tim" 10.0 "cool" connection);
        assert (1 = Redis.zunionstore "union" ["rory"; "tim"] connection);
        assert (1 = Redis.zunionstore "union" ["rory"; "tim"] ~aggregate:`Min connection);
        assert (1 = Redis.zunionstore "union" ["rory"; "tim"] ~aggregate:`Max connection);
        assert (1 = Redis.zunionstore "union" ["rory"] connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "tim"; "10.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZUNIONSTORE union 2 rory tim AGGREGATE SUM");
            WriteThisLine(":1");
            ReadThisLine("ZUNIONSTORE union 2 rory tim AGGREGATE MIN");
            WriteThisLine(":1");
            ReadThisLine("ZUNIONSTORE union 2 rory tim AGGREGATE MAX");
            WriteThisLine(":1");
            ReadThisLine("ZUNIONSTORE union 1 rory AGGREGATE SUM");
            WriteThisLine(":1")
        ])
        test_func;;
        
let test_zunionstore_withweights () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "tim" 10.0 "cool" connection);
        try
            ignore (Redis.zunionstore_withweights "union" ["rory"; "tim"] [1.0] connection)
            with Redis.RedisInvalidArgumentError(_) -> ();
        try
            ignore (Redis.zunionstore_withweights "union" ["rory"; "tim"] [1.0; 0.5; 0.25] connection)
            with Redis.RedisInvalidArgumentError(_) -> ();
        assert (1 = Redis.zunionstore_withweights "union" ["rory"; "tim"] [1.0; 0.5] connection);
        assert (1 = Redis.zunionstore_withweights "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:`Min connection);
        assert (1 = Redis.zunionstore_withweights "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:`Max connection);
        assert (1 = Redis.zunionstore_withweights "union" ["rory"] [1.0] connection)
    in
    use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "tim"; "10.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZUNIONSTORE union 2 rory tim WEIGHTS 1.000000 0.500000 AGGREGATE SUM");
            WriteThisLine(":1");
            ReadThisLine("ZUNIONSTORE union 2 rory tim WEIGHTS 1.000000 0.500000 AGGREGATE MIN");
            WriteThisLine(":1");
            ReadThisLine("ZUNIONSTORE union 2 rory tim WEIGHTS 1.000000 0.500000 AGGREGATE MAX");
            WriteThisLine(":1");
            ReadThisLine("ZUNIONSTORE union 1 rory WEIGHTS 1.000000 AGGREGATE SUM");
            WriteThisLine(":1")
        ])
    test_func;;

let test_zinterstore_withweights () =
    let test_func connection =
        ignore (Redis.zadd "rory" 42.0 "cool" connection);
        ignore (Redis.zadd "tim" 10.0 "cool" connection);
        try
            ignore (Redis.zinterstore_withweights "inter" ["rory"; "tim"] [1.0] connection)
            with Redis.RedisInvalidArgumentError(_) -> ();
        try
            ignore (Redis.zinterstore_withweights "inter" ["rory"; "tim"] [1.0; 0.5; 0.25] connection)
            with Redis.RedisInvalidArgumentError(_) -> ();
        assert (1 = Redis.zinterstore_withweights "inter" ["rory"; "tim"] [1.0; 0.5] connection);
        assert (1 = Redis.zinterstore_withweights "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:`Min connection);
        assert (1 = Redis.zinterstore_withweights "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:`Max connection);
        assert (1 = Redis.zinterstore_withweights "inter" ["rory"] [1.0] connection)
    in
   use_test_script
        ((read_lines_from_list
            ["ZADD"; "rory"; "42.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["ZADD"; "tim"; "10.000000"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("ZINTERSTORE inter 2 rory tim WEIGHTS 1.000000 0.500000 AGGREGATE SUM");
            WriteThisLine(":1");
            ReadThisLine("ZINTERSTORE inter 2 rory tim WEIGHTS 1.000000 0.500000 AGGREGATE MIN");
            WriteThisLine(":1");
            ReadThisLine("ZINTERSTORE inter 2 rory tim WEIGHTS 1.000000 0.500000 AGGREGATE MAX");
            WriteThisLine(":1");
            ReadThisLine("ZINTERSTORE inter 1 rory WEIGHTS 1.000000 AGGREGATE SUM");
            WriteThisLine(":1")
        ])
        test_func;;
