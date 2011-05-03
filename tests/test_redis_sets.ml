(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on sets" *)

open Script;;

let test_sadd () =
    let test_func connection =
        assert (Redis.sadd "rory" "cool" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")])
        test_func;;

let test_srem () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert (Redis.srem "rory" "cool" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SREM"; "rory"; "cool"])
        @ [
            WriteThisLine(":1")
        ])
        test_func;;

let test_spop () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert (Redis.String("cool") = Redis.spop "rory" connection)
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SPOP"; "rory"])
        @ [
            WriteThisLine("$4");
            WriteThisLine("cool");
        ])
        test_func;;

let test_smove () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "even cooler" connection);
        assert ( Redis.smove "tim" "rory" "even cooler" connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SADD"; "tim"; "even cooler"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SMOVE"; "tim"; "rory"; "even cooler"])
        @ [WriteThisLine(":1")])
        test_func;;

let test_scard () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "rory" "even cooler" connection);
        assert ( 2 = Redis.scard "rory" connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SADD"; "rory"; "even cooler"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SCARD"; "rory"])
        @ [WriteThisLine(":2")])
        test_func;;

let test_sismember () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( Redis.sismember "rory" "cool" connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SISMEMBER rory 4");
            ReadThisLine("cool");
            WriteThisLine(":1");
        ])
        test_func;;

let test_smembers () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( [Redis.String("cool")] = Redis.smembers "rory" connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SMEMBERS rory");
            WriteThisLine("*1");
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_sinter () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "cool" connection);
        assert ( [Redis.String("cool")] = Redis.sinter ["rory"; "tim"] connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SADD"; "tim"; "cool"])
        @ [WriteThisLine(":1")]
        @ [
            ReadThisLine("SINTER rory tim");
            WriteThisLine("*1");
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_sinterstore () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "cool" connection);
        assert (1 == Redis.sinterstore "bob" ["rory"; "tim"] connection)
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SADD"; "tim"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SINTERSTORE bob rory tim");
            WriteThisLine(":1")
        ])
        test_func;;

let test_sunion () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "cool" connection);
        assert ([Redis.String("cool")] = Redis.sunion ["rory"; "tim"] connection)
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SADD"; "tim"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SUNION rory tim");
            WriteThisLine("*1");
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_sunionstore () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        ignore (Redis.sadd "tim" "not so cool" connection);
        assert ( 2 = Redis.sunionstore "bob" ["rory"; "tim"] connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [WriteThisLine(":1")]
        @ (read_lines_from_list
            ["SADD"; "tim"; "not so cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SUNIONSTORE bob rory tim");
            WriteThisLine(":2")
        ])
        test_func;;

let test_sdiff () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( [Redis.String("cool")] = Redis.sdiff "rory" ["tim"] connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SDIFF rory tim");
            WriteThisLine("*1");
            WriteThisLine("$4");
            WriteThisLine("cool")
        ])
        test_func;;

let test_sdiffstore () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( 1 = Redis.sdiffstore "bob" "rory" ["tim"] connection )
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SDIFFSTORE bob rory tim");
            WriteThisLine(":1");
        ])
        test_func;;

let test_srandmember () =
    let test_func connection =
        ignore (Redis.sadd "rory" "cool" connection);
        assert ( Redis.String("rory") = (Redis.srandmember "cool" connection))
    in
    use_test_script
        ((read_lines_from_list
            ["SADD"; "rory"; "cool"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SRANDMEMBER cool");
            WriteThisLine("$4");
            WriteThisLine("rory");
        ])
        test_func;;
