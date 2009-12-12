(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on sets" *)
open Script;;

let test_select () =
    let test_func connection =
        Redis.select 1 connection
    in
    use_test_script
        [
            ReadThisLine("SELECT 1");
            WriteThisLine("+OK");
        ]
        test_func;;

let test_move () =
    let test_func connection = 
        Redis.select 0 connection;
        Redis.set "rory" "cool" connection;
        Redis.move "rory" 1 connection
    in
    use_test_script
        [
            ReadThisLine("SELECT 0");
            WriteThisLine("+OK");
            ReadThisLine("SET rory 4");
            ReadThisLine("cool");
            WriteThisLine("+OK");
            ReadThisLine("MOVE rory 1");
            WriteThisLine(":1");
        ]
        test_func;;

let test_flushdb () =
    let test_func connection =
        Redis.flushdb connection
    in
    Script.use_test_script
        [
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
            Script.ReadThisLine("FLUSHALL");
            Script.WriteThisLine("+OK")
        ]
        test_func;;
