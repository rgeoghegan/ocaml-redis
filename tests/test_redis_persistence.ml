(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Persistence control commands" *)

open Script;;

let test_save () =
    let test_func connection =
        Redis.save connection;
    in
    use_test_script
        [
            ReadThisLine("SAVE");
            WriteThisLine("+OK");
        ]
        test_func;;

let test_bgsave () =
    let test_func connection =
        Redis.bgsave connection;
    in
    use_test_script
        [
            ReadThisLine("BGSAVE");
            WriteThisLine("+OK");
        ]
        test_func;;

let test_lastsave () =
    let test_func connection =
        assert ( Big_int.eq_big_int (Big_int.big_int_of_int 42) (Redis.lastsave connection) )
    in
    use_test_script
        [
            ReadThisLine("LASTSAVE");
            WriteThisLine(":42");
        ]
        test_func;;



