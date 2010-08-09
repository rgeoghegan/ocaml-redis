(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Commands operating on hashes" *)

let test_hset () =
    let test_func connection =
        assert(Redis.hset "rory" "cool" "yeah" connection);
        assert( not (Redis.hset "rory" "cool" "no" connection))
    in
    Script.use_test_script
        [
            Script.ReadThisLine("HSET rory cool 4");
            Script.ReadThisLine("yeah");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("HSET rory cool 2");
            Script.ReadThisLine("no");
            Script.WriteThisLine(":0");
        ]
        test_func;;
