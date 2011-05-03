(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on hashes" *)

open Script;;

let test_hset () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "yeah" connection);
        assert( not (Redis.hset "rory" "cool" "no" connection))
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "yeah"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["HSET"; "rory"; "cool"; "no"])
        @ [WriteThisLine(":0")])
        test_func;;

let test_hdel () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "true" connection);
        assert(Redis.hdel "rory" "cool" connection);
        assert(not (Redis.hdel "rory" "cool" connection))
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HDEL rory 4");
            ReadThisLine("cool");
            WriteThisLine(":1");
            ReadThisLine("HDEL rory 4");
            ReadThisLine("cool");
            WriteThisLine(":0")
        ])
        test_func;;

let test_hget () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "true" connection);
        assert(
            (Redis.hget "rory" "cool" connection) = Redis.String("true")
        )
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HGET rory 4");
            ReadThisLine("cool");
            WriteThisLine("$4");
            WriteThisLine("true")
        ])
        test_func;;

let test_hmget () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "true" connection);
        assert(Redis.hset "rory" "handsome" "true" connection);
        assert([Redis.String("true")] = Redis.hmget "rory" ["cool"] connection);
        assert([Redis.String("true"); Redis.String("true")] =
            Redis.hmget "rory" ["cool"; "handsome"] connection)
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["HSET"; "rory"; "handsome"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HMGET rory 4");
            ReadThisLine("cool");
            WriteThisLine("*1");
            WriteThisLine("$4");
            WriteThisLine("true");
            ReadThisLine("HMGET rory cool 8");
            ReadThisLine("handsome");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("true");
            WriteThisLine("$4");
            WriteThisLine("true")
        ])
        test_func;;

let test_hmset () =
    let test_func connection =
        Redis.hmset "rory" [("cool", "true"); ("handsome", "true")] connection
    in
    use_test_script [
        ReadThisLine("*6");
        ReadThisLine("$5");
        ReadThisLine("HMSET");
        ReadThisLine("$4");
        ReadThisLine("rory");
        ReadThisLine("$4");
        ReadThisLine("cool");
        ReadThisLine("$4");
        ReadThisLine("true");
        ReadThisLine("$8");
        ReadThisLine("handsome");
        ReadThisLine("$4");
        ReadThisLine("true");
        WriteThisLine("+OK")
    ] test_func;;

let test_hincrby () =
    let test_func connection =
        assert (Redis.hset "rory" "age" "26" connection);
        assert (27 = Redis.hincrby "rory" "age" 1 connection)
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "age"; "26"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HINCRBY rory age 1");
            WriteThisLine(":27")
        ])
        test_func;;

let test_hexists () =
    let test_func connection =
        assert (Redis.hset "rory" "cool" "true" connection);
        assert (Redis.hexists "rory" "cool" connection);
        assert (not (Redis.hexists "rory" "age" connection))
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HEXISTS rory 4");
            ReadThisLine("cool");
            WriteThisLine(":1");
            ReadThisLine("HEXISTS rory 3");
            ReadThisLine("age");
            WriteThisLine(":0")
        ])
        test_func;;

let test_hlen () =
    let test_func connection =
        assert (Redis.hset "rory" "cool" "true" connection);
        assert (1 = Redis.hlen "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HLEN rory");
            WriteThisLine(":1")
        ])
        test_func;;

let test_hkeys () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "true" connection);
        assert(Redis.hset "rory" "handsome" "true" connection);
        assert(["cool"; "handsome"] = Redis.hkeys "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["HSET"; "rory"; "handsome"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HKEYS rory");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$8");
            WriteThisLine("handsome")
        ])
        test_func;;

let test_hvals () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "true" connection);
        assert(Redis.hset "rory" "handsome" "true" connection);
        assert(["true"; "true"] = Redis.hvals "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["HSET"; "rory"; "handsome"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HVALS rory");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("true");
            WriteThisLine("$4");
            WriteThisLine("true")
        ])
        test_func;;

let test_hgetall () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "true" connection);
        assert(Redis.hset "rory" "handsome" "true" connection);
        assert([("cool", "true"); ("handsome", "true")] = Redis.hgetall "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["HSET"; "rory"; "cool"; "true"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["HSET"; "rory"; "handsome"; "true"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("HGETALL rory");
            WriteThisLine("*4");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$4");
            WriteThisLine("true");
            WriteThisLine("$8");
            WriteThisLine("handsome");
            WriteThisLine("$4");
            WriteThisLine("true")
        ])
    test_func;;
