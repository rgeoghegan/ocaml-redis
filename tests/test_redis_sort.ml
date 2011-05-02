(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for Sorting *)
open Script;;

let test_default () =
    let test_func connection =
        assert (1 = Redis.lpush "rory" "1" connection);
        assert ([Redis.String("1")] = Redis.sort "rory" connection )
    in
    use_test_script
        ((read_lines_from_list
            ["LPUSH"; "rory"; "1"])
        @ [
            WriteThisLine(":1");
            ReadThisLine("SORT rory");
            WriteThisLine("*1");
            WriteThisLine("$1");
            WriteThisLine("1")
        ])
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
                Redis.sort "rory" ~pattern:(Redis.KeyPattern("id_*")) ~limit:(`Limit(0,10));
                Redis.sort "rory" ~pattern:(Redis.KeyPattern("id_*")) ~get:(Redis.KeyPattern("data_*"));
                Redis.sort "rory" ~pattern:(Redis.KeyPattern("id_*")) ~order:`Desc;
                Redis.sort "rory" ~pattern:(Redis.KeyPattern("id_*")) ~alpha:`Alpha;
                Redis.sort "rory" ~pattern:Redis.NoSort ~alpha:`Alpha;
                Redis.sort "rory" ~pattern:(Redis.FieldPattern("hash_*", "id")) ~alpha:`Alpha;
                Redis.sort "rory" ~limit:(`Limit(0,10)) ~get:(Redis.KeyPattern("data_*"));
                Redis.sort "rory" ~limit:(`Limit(0,10)) ~order:`Desc;
                Redis.sort "rory" ~limit:(`Limit(0,10)) ~alpha:`Alpha;
                Redis.sort "rory" ~get:(Redis.KeyPattern("data_*")) ~order:`Desc;
                Redis.sort "rory" ~get:(Redis.KeyPattern("data_*")) ~alpha:`Alpha;
                Redis.sort "rory" ~get:(Redis.FieldPattern("hash_*", "data")) ~alpha:`Alpha;
                Redis.sort "rory" ~order:`Desc ~alpha:`Alpha
            ]
    in
        use_test_script
            [
                ReadThisLine("SORT rory BY id_* LIMIT 0 10");
                WriteThisLine("*0");
                ReadThisLine("SORT rory BY id_* GET data_*");
                WriteThisLine("*0");
                ReadThisLine("SORT rory BY id_* DESC");
                WriteThisLine("*0");
                ReadThisLine("SORT rory BY id_* ALPHA");
                WriteThisLine("*0");
                ReadThisLine("SORT rory BY nosort ALPHA");
                WriteThisLine("*0");
                ReadThisLine("SORT rory BY hash_*->id ALPHA");
                WriteThisLine("*0");

                ReadThisLine("SORT rory LIMIT 0 10 GET data_*");
                WriteThisLine("*0");
                ReadThisLine("SORT rory LIMIT 0 10 DESC");
                WriteThisLine("*0");
                ReadThisLine("SORT rory LIMIT 0 10 ALPHA");
                WriteThisLine("*0");

                ReadThisLine("SORT rory GET data_* DESC");
                WriteThisLine("*0");
                ReadThisLine("SORT rory GET data_* ALPHA");
                WriteThisLine("*0");
                ReadThisLine("SORT rory GET hash_*->data ALPHA");
                WriteThisLine("*0");

                ReadThisLine("SORT rory DESC ALPHA");
                WriteThisLine("*0");
            ]
            test_func;;

let test_sort_get_many_get_one () =
    let test_func connection = 
        begin
            assert (1 = Redis.rpush "people" "1" connection);
            Redis.set "age_1" "25" connection;
            Redis.set "name_1" "rory" connection;
            assert (2 = Redis.rpush "people" "2" connection);
            Redis.set "age_2" "20" connection;
            Redis.set "name_2" "tim" connection;
            assert (
                [   [ Redis.String("20"); Redis.String("tim") ];
                    [ Redis.String("25"); Redis.String("rory") ] ]
                = Redis.sort_get_many "people" ["age_*"; "name_*"]
                    ~pattern:(Redis.KeyPattern("age_*")) connection
            )
        end
    in
        use_test_script
            ((read_lines_from_list
                ["RPUSH"; "people"; "1"])
            @ [WriteThisLine(":1")]
            @ (read_lines_from_list
                ["SET"; "age_1"; "25"])
            @ [WriteThisLine("+OK")]
            @ (read_lines_from_list
                ["SET"; "name_1"; "rory"])
            @ [WriteThisLine("+OK")]
            @ (read_lines_from_list
                ["RPUSH"; "people"; "2"])
            @ [WriteThisLine(":2")]
            @ (read_lines_from_list
                ["SET"; "age_2"; "20"])
            @ [WriteThisLine("+OK")]
            @ (read_lines_from_list
                ["SET"; "name_2"; "tim"])
            @ [WriteThisLine("+OK")]
            @ [
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
            ])
            test_func;;

let test_sort_and_store () =
    let test_func connection = 
        begin
            assert (1 = Redis.rpush "people" "1" connection);
            Redis.set "age_1" "25" connection;
            Redis.set "name_1" "rory" connection;
            assert (2 = Redis.rpush "people" "2" connection);
            Redis.set "age_2" "20" connection;
            Redis.set "name_2" "tim" connection;
            assert ( 4 = Redis.sort_and_store
                            "people"
                            ["age_*"; "name_*"]
                            "results"
                            ~pattern:(Redis.KeyPattern("age_*"))
                            connection
            )
        end
    in
        use_test_script
            ((read_lines_from_list
                ["RPUSH"; "people"; "1"])
            @ [WriteThisLine(":1")]
            @ (read_lines_from_list
                ["SET"; "age_1"; "25"])
            @ [WriteThisLine("+OK")]
            @ (read_lines_from_list
                ["SET"; "name_1"; "rory"])
            @ [WriteThisLine("+OK")]
            @ (read_lines_from_list
                ["RPUSH"; "people"; "2"])
            @ [WriteThisLine(":2")]
            @ (read_lines_from_list
                ["SET"; "age_2"; "20"])
            @ [WriteThisLine("+OK")]
            @ (read_lines_from_list
                ["SET"; "name_2"; "tim"])
            @ [
                WriteThisLine("+OK");
                ReadThisLine("SORT people BY age_* GET age_* GET name_* STORE results");
                WriteThisLine(":4")
            ])
            test_func;;
