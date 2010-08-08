(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on lists" *)

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
    Script.use_test_script
        [
            Script.ReadThisLine("RPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("SET rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("+OK");
            Script.ReadThisLine("RPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("-ERR Operation against a key holding the wrong kind of value");
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
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_llen () =
    let test_func connection =
        assert (1 = Redis.rpush "rory" "cool" connection);
        assert (2 = Redis.rpush "rory" "still cool" connection);
        assert ( [Redis.String("cool"); Redis.String("still cool")] = (Redis.lrange "rory" 0 1 connection))
    in
    Script.use_test_script
        [
            Script.ReadThisLine("RPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("RPUSH rory 10");
            Script.ReadThisLine("still cool");
            Script.WriteThisLine(":2");
            Script.ReadThisLine("LRANGE rory 0 1");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.WriteThisLine("$10");
            Script.WriteThisLine("still cool");
        ]
        test_func;;

let test_ltrim () =
    let test_func connection =
        assert (1 = Redis.rpush "rory" "cool" connection);
        assert (2 = Redis.rpush "rory" "still cool" connection);
        Redis.ltrim "rory" 0 1 connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("RPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("RPUSH rory 10");
            Script.ReadThisLine("still cool");
            Script.WriteThisLine(":2");
            Script.ReadThisLine("LTRIM rory 0 1");
            Script.WriteThisLine("+OK");
        ]
        test_func;;

let test_lset () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        Redis.lset "rory" 0 "even cooler" connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("LSET rory 0 11");
            Script.ReadThisLine("even cooler");
            Script.WriteThisLine("+OK");
        ]
        test_func;;

let test_lrem () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (2 = Redis.lpush "rory" "even cooler" connection);
        assert (1 == Redis.lrem "rory" 0 "cool" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("LPUSH rory 11");
            Script.ReadThisLine("even cooler");
            Script.WriteThisLine(":2");
            Script.ReadThisLine("LREM rory 0 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
        ]
        test_func;;

let test_lpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.lpop "rory" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("LPOP rory");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
        ]
        test_func;;

let test_rpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.rpop "rory" connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("RPOP rory");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
        ]
        test_func;;

let test_rpoplpush () =
    let test_func connection =
        assert (1 = Redis.lpush "cool" "rory" connection);
        assert (Redis.String("rory") = (Redis.rpoplpush "cool" "not_cool" connection))
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH cool 4");
            Script.ReadThisLine("rory");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("RPOPLPUSH cool not_cool");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory");
        ]
        test_func;;

let test_blpop () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.blpop "rory" connection);
        assert (Redis.Nil = Redis.blpop "rory" ~timeout:(`Seconds(3)) connection)
    in
    Script.use_test_script
        [
            Script.ReadThisLine("LPUSH rory 4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("BLPOP rory 0");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.ReadThisLine("BLPOP rory 3");
            Script.WriteThisLine("*-1")
        ]
        test_func;;
