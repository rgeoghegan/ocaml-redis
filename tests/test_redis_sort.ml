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
            WriteThisLine(":1");
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

let test_sort_get_many_get_one () =
    let test_func connection = 
        begin
            Redis.rpush "people" "1" connection;
            Redis.set "age_1" "25" connection;
            Redis.set "name_1" "rory" connection;
            Redis.rpush "people" "2" connection;
            Redis.set "age_2" "20" connection;
            Redis.set "name_2" "tim" connection;
            assert (
                [   [ Redis.String("20"); Redis.String("tim") ];
                    [ Redis.String("25"); Redis.String("rory") ] ]
                = Redis.sort_get_many "people" ["age_*"; "name_*"]
                    ~pattern:"age_*" connection
            )
        end
    in
        use_test_script
            [
                ReadThisLine("RPUSH people 1");
                ReadThisLine("1");
                WriteThisLine(":1");
                ReadThisLine("SET age_1 2");
                ReadThisLine("25");
                WriteThisLine("+OK");
                ReadThisLine("SET name_1 4");
                ReadThisLine("rory");
                WriteThisLine("+OK");
                ReadThisLine("RPUSH people 1");
                ReadThisLine("2");
                WriteThisLine(":2");
                ReadThisLine("SET age_2 2");
                ReadThisLine("20");
                WriteThisLine("+OK");
                ReadThisLine("SET name_2 3");
                ReadThisLine("tim");
                WriteThisLine("+OK");
                ReadThisLine("SORT people BY age_* GET age_* GET name_*");
                WriteThisLine("*4");
                WriteThisLine("$2");
                WriteThisLine("20");
                WriteThisLine("$3");
                WriteThisLine("tim");
                WriteThisLine("$2");
                WriteThisLine("25");
                WriteThisLine("$4");
                WriteThisLine("rory")
            ]
            test_func;;

let test_sort_and_store () =
    let test_func connection = 
        begin
            Redis.rpush "people" "1" connection;
            Redis.set "age_1" "25" connection;
            Redis.set "name_1" "rory" connection;
            Redis.rpush "people" "2" connection;
            Redis.set "age_2" "20" connection;
            Redis.set "name_2" "tim" connection;
            assert ( 4 = Redis.sort_and_store
                            "people"
                            ["age_*"; "name_*"]
                            "results"
                            ~pattern:"age_*"
                            connection
            )
        end
    in
        use_test_script
            [
                ReadThisLine("RPUSH people 1");
                ReadThisLine("1");
                WriteThisLine(":1");
                ReadThisLine("SET age_1 2");
                ReadThisLine("25");
                WriteThisLine("+OK");
                ReadThisLine("SET name_1 4");
                ReadThisLine("rory");
                WriteThisLine("+OK");
                ReadThisLine("RPUSH people 1");
                ReadThisLine("2");
                WriteThisLine(":2");
                ReadThisLine("SET age_2 2");
                ReadThisLine("20");
                WriteThisLine("+OK");
                ReadThisLine("SET name_2 3");
                ReadThisLine("tim");
                WriteThisLine("+OK");
                ReadThisLine("SORT people BY age_* GET age_* GET name_* STORE results");
                WriteThisLine(":4");
            ]
            test_func;;
