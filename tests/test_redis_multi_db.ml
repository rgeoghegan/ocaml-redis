(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on multiple db" *)
open Script;;

let test_select () =
    let test_func connection =
        Redis.select 1 connection
    in
    use_test_script
        ((read_lines_from_list
            ["SELECT"; "1"])
        @ [WriteThisLine("+OK")])
        test_func;;

let test_move () =
    let test_func connection = 
        Redis.select 0 connection;
        Redis.set "rory" "cool" connection;
        Redis.move "rory" 1 connection
    in
    use_test_script
        ((read_lines_from_list
            ["SELECT"; "0"])
        @ [WriteThisLine("+OK")]
        @ (read_lines_from_list
            ["SET"; "rory"; "cool"])
        @ [WriteThisLine("+OK")]
        @ (read_lines_from_list
            ["MOVE"; "rory"; "1"])
        @ [WriteThisLine(":1")])
        test_func;;

let test_flushdb () =
    let test_func connection =
        Redis.flushdb connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("*1");
            Script.ReadThisLine("$7");
            Script.ReadThisLine("FLUSHDB");
            Script.WriteThisLine("+OK")
        ]
        test_func;;

let test_flushall () =
    let test_func connection =
        Redis.flushall connection;
    in
    Script.use_test_script
        [
            Script.ReadThisLine("*1");
            Script.ReadThisLine("$8");
            Script.ReadThisLine("FLUSHALL");
            Script.WriteThisLine("+OK")
        ]
        test_func;;
