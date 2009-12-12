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
