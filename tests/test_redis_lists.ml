(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on lists" *)

open Script;;

let test_rpush () =
    let test_func connection =
        assert( 1 == (Redis.rpush "rory" "cool" connection));
        Redis.set "rory" "cool" connection;
        try
            begin
                ignore (Redis.rpush "rory" "cool" connection);
                failwith "Should have thrown error about pushing to list."
            end
        with Redis.RedisServerError(_) -> ()
    in
    use_test_script
        ((read_lines_from_list
            ["RPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [WriteThisLine("+OK")]
        @ (read_lines_from_list
            ["RPUSH"; "rory"; "cool"])
        @ [WriteThisLine("-ERR Operation against a key holding the wrong kind of value")])
        test_func;;

let test_lpush () =
    let test_func connection =
        Redis.lpush "rory" "cool" connection
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [
            WriteThisLine(":1")
        ])
        test_func;;

let test_llen () =
    let test_func connection =
        assert (1 = Redis.rpush "rory" "cool" connection);
        assert (1 = Redis.llen "rory" connection);
        assert (2 = Redis.rpush "rory" "still cool" connection);
        assert (2 = Redis.llen "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["RPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["LLEN"; "rory"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["RPUSH"; "rory"; "still cool"])
        @ [WriteThisLine(":2")]
        @ (read_lines_from_list
            ["LLEN"; "rory"])
        @ [WriteThisLine(":2")])
        test_func;;
    
let test_lrange () =
    let test_func connection =
        assert (1 = Redis.rpush "rory" "cool" connection);
        assert (2 = Redis.rpush "rory" "still cool" connection);
        assert ( [Redis.String("cool"); Redis.String("still cool")] = (Redis.lrange "rory" 0 1 connection))
    in
    use_test_script
        ((read_lines_from_list
            ["RPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["RPUSH"; "rory"; "still cool"])
        @ [WriteThisLine(":2")]
        @ (read_lines_from_list
            ["LRANGE"; "rory"; "0"; "1"])
        @ [
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("cool");
            WriteThisLine("$10");
            WriteThisLine("still cool")
        ])
        test_func;;

let test_ltrim () =
    let test_func connection =
        assert (1 = Redis.rpush "rory" "cool" connection);
        assert (2 = Redis.rpush "rory" "still cool" connection);
        Redis.ltrim "rory" 0 1 connection
    in
    use_test_script
        ((read_lines_from_list
            ["RPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["RPUSH"; "rory"; "still cool"])
        @ [WriteThisLine(":2")]
        @ (read_lines_from_list
            ["LTRIM"; "rory"; "0"; "1"])
        @ [WriteThisLine("+OK")])
        test_func;;

let test_lindex () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.lindex "rory" 0 connection);
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["LINDEX"; "rory"; "0"])
        @ [
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_lset () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        Redis.lset "rory" 0 "even cooler" connection
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["LSET"; "rory"; "0"; "even cooler"])
        @ [WriteThisLine("+OK")])
        test_func;;

let test_lrem () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (2 = Redis.lpush "rory" "even cooler" connection);
        assert (1 == Redis.lrem "rory" 0 "cool" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["LPUSH"; "rory"; "even cooler"])
        @ [WriteThisLine(":2")]
        @ (read_lines_from_list
            ["LREM"; "rory"; "0"; "cool"])
        @ [WriteThisLine(":1")])
        test_func;;

let test_lpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.lpop "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["LPOP"; "rory"])
        @ [
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_rpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.rpop "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["RPOP"; "rory"])
        @ [
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_rpoplpush () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("rory") = (Redis.rpoplpush "cool" "not_cool" connection))
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["RPOPLPUSH"; "cool"; "not_cool"])
        @ [
            WriteThisLine("$4");
            WriteThisLine("rory")
        ])
        test_func;;

let test_blpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.blpop "rory" connection);
        assert (Redis.Nil = Redis.blpop "rory" ~timeout:(`Seconds(3)) connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["BLPOP"; "rory"; "0"])
        @ [
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("rory");
            WriteThisLine("$4");
            WriteThisLine("cool");
        ]
        @ (read_lines_from_list
            ["BLPOP"; "rory"; "3"])
        @ [WriteThisLine("*-1")])
        test_func;;

let test_blpop_many () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (("rory", Redis.String("cool")) = Redis.blpop_many ["rory"; "tim"; "bob"] connection);
        assert (("", Redis.Nil) = Redis.blpop_many ["rory"; "tim"; "bob"] ~timeout:(`Seconds(3)) connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("BLPOP rory tim bob 0");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("rory");
            WriteThisLine("$4");
            WriteThisLine("cool");
            ReadThisLine("BLPOP rory tim bob 3");
            WriteThisLine("*-1")
        ])
        test_func;;

let test_brpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.brpop "rory" connection);
        assert (Redis.Nil = Redis.brpop "rory" ~timeout:(`Seconds(3)) connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("BRPOP rory 0");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("rory");
            WriteThisLine("$4");
            WriteThisLine("cool");
            ReadThisLine("BRPOP rory 3");
            WriteThisLine("*-1")
        ])
        test_func;;

let test_brpop_many () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (("rory", Redis.String("cool")) = Redis.brpop_many ["rory"; "tim"; "bob"] connection);
        assert (("", Redis.Nil) = Redis.brpop_many ["rory"; "tim"; "bob"] ~timeout:(`Seconds(3)) connection)
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("BRPOP rory tim bob 0");
            WriteThisLine("*2");
            WriteThisLine("$4");
            WriteThisLine("rory");
            WriteThisLine("$4");
            WriteThisLine("cool");
            ReadThisLine("BRPOP rory tim bob 3");
            WriteThisLine("*-1")
        ])
        test_func;;
