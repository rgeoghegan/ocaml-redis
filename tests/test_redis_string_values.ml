(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on string values" *)

let test_set () =
    let test_func connection =
        Redis.set "key" "aaa" connection
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "key"; "aaa"])
        @ [
            Script.WriteThisLine("+OK")
        ])
        test_func;;

let test_get () =
    let test_func connection = 
        assert (
            (Redis.get "key" connection) = Redis.String("aaa")
        )
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["GET"; "key"])
        @ [
            Script.WriteThisLine("$3");
            Script.WriteThisLine("aaa")
        ])
        test_func;;

let test_getset () =
    let test_func connection =
        assert (
            (Redis.getset "key" "now" connection) = Redis.String("previous")
        )
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["GETSET"; "key"; "now"])
        @ [
            Script.WriteThisLine("$8");
            Script.WriteThisLine("previous")
        ])
        test_func;;

let test_mget () =
    let test_func conn =
        assert (
            (Redis.mget ["rory"; "tim"] conn) = [Redis.String("cool"); Redis.String("not cool")]
        )
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["MGET"; "rory"; "tim"])
        @ [
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.WriteThisLine("$8");
            Script.WriteThisLine("not cool")
        ])
        test_func;;

let test_setnx () =
    let test_func connection =
        begin
            assert( Redis.setnx "key" "aaa" connection );
            assert( false = Redis.setnx "key" "aaa" connection)
        end
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SETNX"; "key"; "aaa"])
        @ [ Script.WriteThisLine(":1") ]
        @ (Script.read_lines_from_list
            ["SETNX"; "key"; "aaa"])
        @ [Script.WriteThisLine(":0")])
        test_func;;

let test_setex () =
    let test_func connection =
        Redis.setex "key" 10 "aaa" connection
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SETEX"; "key"; "10"; "aaa"])
        @ [ 
            Script.WriteThisLine("+OK")
        ])
        test_func;;

let test_mset () =
    let test_func connection =
        Redis.mset [("rory", "cool"); ("tim", "not cool")] connection
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            (* These values come out backwards because of the way the list is unwrapped *)
            ["MSET"; "tim"; "not cool"; "rory"; "cool"])
        @ [
            Script.WriteThisLine("+OK");
        ])
        test_func;;

let test_msetnx () =
    let test_func connection =
        begin
            assert(Redis.msetnx [("rory", "cool"); ("tim", "not cool")] connection);
            assert(false = Redis.msetnx [("rory", "cool"); ("tim", "not cool")] connection)
        end
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            (* These values come out backwards because of the way the list is unwrapped *)
            ["MSETNX"; "tim"; "not cool"; "rory"; "cool"])
        @ [Script.WriteThisLine(":1")]
        @ (Script.read_lines_from_list
            ["MSETNX"; "tim"; "not cool"; "rory"; "cool"])
        @ [Script.WriteThisLine(":0")]
        )
        test_func;;

let test_incr () =
    let test_func connection =
        begin
            assert( 1 = Redis.incr "key" connection );
            assert( 2 = Redis.incr "key" connection)
        end
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["INCR"; "key"])
        @ [ Script.WriteThisLine(":1")]
        @ (Script.read_lines_from_list ["INCR"; "key"])
        @ [ Script.WriteThisLine(":2")])
        test_func;;

let test_incrby () =
    let test_func connection =
        begin
            assert( 2 = Redis.incrby "key" 2 connection );
            assert( 4 = Redis.incrby "key" 2 connection)
        end
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["INCRBY"; "key"; "2"])
        @ [Script.WriteThisLine(":2")]
        @ (Script.read_lines_from_list
            ["INCRBY"; "key"; "2"])
        @ [Script.WriteThisLine(":4")])
        test_func;;

let test_decr () =
    let test_func connection =
        begin
            assert( -1 = Redis.decr "key" connection );
            assert( -2 = Redis.decr "key" connection)
        end
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["DECR"; "key"])
        @ [Script.WriteThisLine(":-1")]
        @ (Script.read_lines_from_list
            ["DECR"; "key"])
        @ [Script.WriteThisLine(":-2")])
        test_func;;

let test_decrby () =
    let test_func connection =
        begin
            assert( -2 = Redis.decrby "key" 2 connection );
            assert( -4 = Redis.decrby "key" 2 connection)
        end
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["DECRBY"; "key"; "2"])
        @ [Script.WriteThisLine(":-2")]
        @ (Script.read_lines_from_list
            ["DECRBY"; "key"; "2"])
        @ [Script.WriteThisLine(":-4")])
        test_func;;

let test_exists () =
    let test_func connection = begin
        assert (
            (Redis.exists "real_key" connection) = true
        );
        assert (
            (Redis.exists "fake_key" connection) = false
        )
    end in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["EXISTS"; "real_key"])
        @ [Script.WriteThisLine(":1")]
        @ (Script.read_lines_from_list
            ["EXISTS"; "fake_key"])
        @ [Script.WriteThisLine(":0")])
        test_func;;

let test_del () =
    let test_func connection = begin
        Redis.set "rory" "cool" connection;
        Redis.set "tim" "uncool" connection;
        Redis.set "bob" "unknown" connection;
        assert (1 == Redis.del ["bob"] connection);
        assert (2 == Redis.del ["rory"; "tim"; "bob"] connection);
    end in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["SET"; "tim"; "uncool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["SET"; "bob"; "unknown"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["DEL"; "bob"])
        @ [Script.WriteThisLine(":1")]
        @ (Script.read_lines_from_list
            ["DEL"; "rory"; "tim"; "bob"])
        @ [Script.WriteThisLine(":2")]
        )
        test_func;;
    
let test_value_type () =
    let test_func connection = begin
        Redis.set "rory" "cool" connection;
        ignore (Redis.zadd "tim" 1.0 "not cool" connection);
        assert (Redis.RedisString = Redis.value_type "rory" connection);
        assert (Redis.RedisZSet = Redis.value_type "tim" connection);
        assert (Redis.RedisNil = Redis.value_type "bob" connection);
    end in
    Script.use_test_script
        [
            Script.ReadThisLine("*3");
            Script.ReadThisLine("$3");
            Script.ReadThisLine("SET");
            Script.ReadThisLine("$4");
            Script.ReadThisLine("rory");
            Script.ReadThisLine("$4");
            Script.ReadThisLine("cool");
            Script.WriteThisLine("+OK");
            Script.ReadThisLine("ZADD tim 1.000000 8");
            Script.ReadThisLine("not cool");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("TYPE rory");
            Script.WriteThisLine("+string");
            Script.ReadThisLine("TYPE tim");
            Script.WriteThisLine("+zset");
            Script.ReadThisLine("TYPE bob");
            Script.WriteThisLine("+none");
        ]
        test_func;;

let test_append () =
    let test_func connection =
        assert (4 = Redis.append "rory" "cool" connection);
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["APPEND"; "rory"; "cool"])
        @ [
            Script.WriteThisLine(":4")
        ])
        test_func;;
    
let test_substr () =
    let test_func connection =
        Redis.set "rory" "cool" connection;
        assert (Redis.String("ol") = Redis.substr "rory" 2 4 connection)
    in
    Script.use_test_script 
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["SUBSTR"; "rory"; "2"; "4"])
        @ [
            Script.WriteThisLine("$2");
            Script.WriteThisLine("ol")
        ])
    test_func;;
