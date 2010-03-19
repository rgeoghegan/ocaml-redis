(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for Sorting *)
open Script;;

let test_default () =
    let test_func connection =
        Redis.lpush "rory" "1" connection;
        assert( [Redis.String("1")] = Redis.sort "rory" connection )
    in
    use_test_script
        [
            ReadThisLine("LPUSH rory 1");
            ReadThisLine("1");
            WriteThisLine("+OK");
            ReadThisLine("SORT rory");
            WriteThisLine("*1");
            WriteThisLine("$1");
            WriteThisLine("1");
        ]
        test_func;;

let test_sort_all_combos () =
    let test_func connection =
        let rec iter f = 
            match f with
                [] -> () |
                h :: t -> begin
                    assert ([] = h connection );
                    iter t
                end
        in
            iter [
                Redis.sort "rory" ~pattern:"id_*" ~limit:(`Limit(0,10));
                Redis.sort "rory" ~pattern:"id_*" ~get:"data_*";
                Redis.sort "rory" ~pattern:"id_*" ~order:`Desc;
                Redis.sort "rory" ~pattern:"id_*" ~alpha:`Alpha;
                Redis.sort "rory" ~limit:(`Limit(0,10)) ~get:"data_*";
                Redis.sort "rory" ~limit:(`Limit(0,10)) ~order:`Desc;
                Redis.sort "rory" ~limit:(`Limit(0,10)) ~alpha:`Alpha;
                Redis.sort "rory" ~get:"data_*" ~order:`Desc;
                Redis.sort "rory" ~get:"data_*" ~alpha:`Alpha;
                Redis.sort "rory" ~order:`Desc ~alpha:`Alpha
            ]
    in
        use_test_script
            [
                ReadThisLine("SORT rory BY id_* LIMIT 0 10");
                WriteThisLine("*-1");
                ReadThisLine("SORT rory BY id_* GET data_*");
                WriteThisLine("*-1");
                ReadThisLine("SORT rory BY id_* DESC");
                WriteThisLine("*-1");
                ReadThisLine("SORT rory BY id_* ALPHA");
                WriteThisLine("*-1");

                ReadThisLine("SORT rory LIMIT 0 10 GET data_*");
                WriteThisLine("*-1");
                ReadThisLine("SORT rory LIMIT 0 10 DESC");
                WriteThisLine("*-1");
                ReadThisLine("SORT rory LIMIT 0 10 ALPHA");
                WriteThisLine("*-1");

                ReadThisLine("SORT rory GET data_* DESC");
                WriteThisLine("*-1");
                ReadThisLine("SORT rory GET data_* ALPHA");
                WriteThisLine("*-1");

                ReadThisLine("SORT rory DESC ALPHA");
                WriteThisLine("*-1");
            ]
            test_func;;
