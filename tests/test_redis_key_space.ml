(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on the key space" *)

let test_keys () =
    let test_func connection =
        Redis.set "rory" "cool" connection;
        Redis.set "tim" "uncool" connection;
        assert(["rory"; "tim"] = Redis.keys "*" connection)
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["SET"; "tim"; "uncool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["KEYS"; "*"])
        @ [
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory");
            Script.WriteThisLine("$3");
            Script.WriteThisLine("tim")
        ])
        test_func;;

let test_randomkey () =
    let test_func connection =
        Redis.set "rory" "cool" connection;
        assert("rory" = Redis.randomkey connection)
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [
            Script.WriteThisLine("+OK");
            Script.ReadThisLine("*1");
            Script.ReadThisLine("$9");
            Script.ReadThisLine("RANDOMKEY");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory")
        ])
        test_func;;

let test_rename () =
    let test_func connection = 
        Redis.set "rory" "cool" connection;
        Redis.rename "rory" "tim" connection
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["RENAME"; "rory"; "tim"])
        @ [Script.WriteThisLine("+OK")])
        test_func;;

let test_renamenx () =
    let test_func connection = 
        Redis.set "rory" "cool" connection;
        Redis.set "tim" "not cool" connection;
        assert( false == Redis.renamenx "rory" "tim" connection);
        assert(Redis.renamenx "rory" "bob" connection)
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["SET"; "tim"; "not cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["RENAMENX"; "rory"; "tim"])
        @ [Script.WriteThisLine(":0")]
        @ (Script.read_lines_from_list
            ["RENAMENX"; "rory"; "bob"])
        @ [Script.WriteThisLine(":1")])
        test_func;;

let test_dbsize () =
    let test_func connection = 
        assert( 0 == Redis.dbsize connection);
    in
    Script.use_test_script
        [
            Script.ReadThisLine("*1");
            Script.ReadThisLine("$6");
            Script.ReadThisLine("DBSIZE");
            Script.WriteThisLine(":0")
        ]
        test_func;;

let test_expire () =
    let test_func connection =
        Redis.set "rory" "cool" connection;
        assert( Redis.expire "rory" 10 connection)
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["EXPIRE"; "rory"; "10"])
        @ [Script.WriteThisLine(":1")])
        test_func;;

let test_expireat () =
    let test_func connection =
        Redis.set "rory" "cool" connection;
        Redis.expireat "rory" 946765012. connection
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["EXPIREAT"; "rory"; "946765012"])
        @ [Script.WriteThisLine(":1")])
        test_func;;

let test_ttl () =
    let test_func connection =
        Redis.set "rory" "cool" connection;
        ignore( Redis.expire "rory" 10 connection);
        assert( 10 == Redis.ttl "rory" connection)
    in
    Script.use_test_script
        ((Script.read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [Script.WriteThisLine("+OK")]
        @ (Script.read_lines_from_list
            ["EXPIRE"; "rory"; "10"])
        @ [
            Script.WriteThisLine(":1");
            Script.ReadThisLine("TTL rory");
            Script.WriteThisLine(":10")
        ])
        test_func;;
