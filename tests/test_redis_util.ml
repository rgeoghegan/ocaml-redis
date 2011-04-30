(* Copyright (C) 2010 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Test utility functions in redis.ml *)

open Script

let test_string_of_bulk_data () =
    begin
        assert ( "rory" = (Redis_common.string_of_bulk_data
            (Redis_common.String("rory"))));
        try
            (* Test failure when passing in Nil *)
            ignore (Redis_common.string_of_bulk_data (Redis_common.Nil))
        with Redis_common.RedisNilError(_) -> ()
    end;;

let test_int_of_rank () =
    begin
        assert ( 42 = (Redis_common.int_of_rank (Redis_common.Rank(42))));
        try
            (* Test failure when passing in Nil *)
            ignore (Redis_common.int_of_rank (Redis_common.NilRank))
        with Redis_common.RedisNilError(_) -> ()
    end;;

let run_comparison_tests test_data transformation_function =
    let tester (a,b) = 
        assert ( transformation_function a = b )
    in
        List.iter tester test_data;;

let test_string_of_response () =
        run_comparison_tests
            [
                (Redis_common.Status("rory"), "Status(\"rory\")");
                (Redis_common.Undecipherable, "Undecipherable");
                (Redis_common.Integer(42), "Integer(42)");
                (Redis_common.LargeInteger(42.0), "LargeInteger(42.00)");
                (Redis_common.Bulk(Redis_common.Nil), "Bulk(Nil)");
                (Redis_common.Multibulk(Redis_common.MultibulkValue([Redis_common.String("rory"); Redis_common.Nil])), "Multibulk([String(\"rory\"); Nil])")
            ]
            Redis_common.string_of_response;;

let test_string_of_redis_value_type () =
    run_comparison_tests
        [
            (Redis_common.RedisString , "String");
            (Redis_common.RedisNil , "Nil");
            (Redis_common.RedisList , "List");
            (Redis_common.RedisSet, "Set");
            (Redis_common.RedisZSet, "ZSet");
        ]
        Redis_common.string_of_redis_value_type;;

let test_connection_read_string () =
    let test_pipe_read, test_pipe_write = piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\n";
        flush test_pipe_write;
        assert (
            Redis_common.Connection.read_string (test_pipe_read, test_pipe_write)
            = "test string"
        );
        output_string test_pipe_write "test string\rmore test string\neven more test string\r\n";
        flush test_pipe_write;
        assert (
            Redis_common.Connection.read_string (test_pipe_read, test_pipe_write)
            = "test string\rmore test string\neven more test string"
        )
    end;;

let test_connection_read_fixed_string () =
    let test_pipe_read, test_pipe_write = piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\na bit more";
        flush test_pipe_write;
        assert (
            Redis_common.Connection.read_fixed_string 10 (test_pipe_read, test_pipe_write)
            = "test strin"
        )
    end;;

let test_connection_send_text () =
    let test_func connection =
        Redis_common.Connection.send_text "foo" connection
    in
    use_test_script [ReadThisLine("foo")] test_func;;

let test_get_bulk_data () =
    let test_func connection =
        assert(Redis_common.String("") = Redis_common.Helpers.get_bulk_data connection);
        assert(Redis_common.String("test") = Redis_common.Helpers.get_bulk_data connection);
        assert(Redis_common.Nil = Redis_common.Helpers.get_bulk_data connection)
    in
    use_test_script [
        WriteThisLine("0");
        WriteThisLine("");
        WriteThisLine("4");
        WriteThisLine("test");
        WriteThisLine("-1")
    ] test_func;;

let test_get_multibulk_data () =
    let test_func connection =
        assert(Redis_common.Multibulk(Redis_common.MultibulkNil) = (Redis_common.Helpers.get_multibulk_data (-1) connection));
        assert(Redis_common.Multibulk(Redis_common.MultibulkValue([])) = Redis_common.Helpers.get_multibulk_data 0 connection);
        assert(Redis_common.Multibulk(Redis_common.MultibulkValue([Redis_common.String("foo")])) 
            = Redis_common.Helpers.get_multibulk_data 1 connection);
        assert(Redis_common.Multibulk(Redis_common.MultibulkValue([Redis_common.String("foo"); Redis_common.String("bar")]))
            = Redis_common.Helpers.get_multibulk_data 2 connection);
        assert(Redis_common.Multibulk(Redis_common.MultibulkValue([Redis_common.Nil]))
            = Redis_common.Helpers.get_multibulk_data 1 connection);
        assert(Redis_common.Multibulk(Redis_common.MultibulkValue([Redis_common.Nil; Redis_common.String("bar")]))
            = Redis_common.Helpers.get_multibulk_data 2 connection)
    in
    use_test_script [
        WriteThisLine("$3");
        WriteThisLine("foo");

        WriteThisLine("$3");
        WriteThisLine("foo");
        WriteThisLine("$3");
        WriteThisLine("bar");

        WriteThisLine("$-1");

        WriteThisLine("$-1");
        WriteThisLine("$3");
        WriteThisLine("bar");
    ] test_func;;

let test_parse_integer_response () =
    assert( Redis_common.Integer(42) = Redis_common.Helpers.parse_integer_response "42");
    assert( Redis_common.LargeInteger(10000000000000000000.0) = Redis_common.Helpers.parse_integer_response "10000000000000000000");;

let test_handle_error () =
    assert (Redis_common.Status("OK") = (Redis_common.Helpers.handle_error (Redis_common.Status("OK"))));
    try ignore (Redis_common.Helpers.handle_error (Redis_common.Error("Some error")));
            failwith ("Should have raised error")
        with Redis_common.RedisServerError(x) ->
            assert( x = "Some error" );
    try ignore (Redis_common.Helpers.handle_error (Redis_common.Undecipherable));
            failwith ("Should have raised error")
        with Redis_common.RedisServerError(x) ->
            assert(x = "Could not decipher response");;

let test_receive_answer () =
    let test_func connection =
        begin
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Status("bar")
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Error("baz")
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Undecipherable
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Integer(42)
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.LargeInteger(18446744073709551616.0)
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Bulk(Redis_common.String("aaa"))
            );
            assert(
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Bulk(Redis_common.String("I contain\r\na line split"))
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Bulk(Redis_common.Nil)
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Multibulk(Redis_common.MultibulkValue([Redis_common.String("rory"); Redis_common.String("tim")]))
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Multibulk(Redis_common.MultibulkValue([]))
            );
            assert (
                Redis_common.Helpers.receive_answer connection
                = Redis_common.Multibulk(Redis_common.MultibulkNil)
            )
        end
    in
    use_test_script 
        [
            WriteThisLine("+bar"); (* Status *)
            WriteThisLine("-baz"); (* Error *)
            WriteThisLine("!"); (* Undecipherable *)
            WriteThisLine(":42"); (* Integer *)
            WriteThisLine(":18446744073709551616"); (* LargeInteger *)
            WriteThisLine("$3"); (* Bulk *)
            WriteThisLine("aaa");
            WriteThisLine("$23");
            WriteThisLine("I contain\r\na line split");
            WriteThisLine("$-1"); (* Nil Bulk *)
            WriteThisLine("*2"); (* Multibulk *)
            WriteThisLine("$4");
            WriteThisLine("rory");
            WriteThisLine("$3");
            WriteThisLine("tim");
            WriteThisLine("*0"); (* Empty Multibulk *)
            WriteThisLine("*-1"); (* Nil Multibulk *)
        ]
        test_func;;

let test_send_and_receive_command () =
    let test_func connection =
        begin
            assert (
                Redis_common.Helpers.send_and_receive_command "foo" connection
                = Redis_common.Status("bar")
            );
            assert (
                Redis_common.Helpers.send_and_receive_command "foo" connection
                = Redis_common.Undecipherable
            );
            assert (
                Redis_common.Helpers.send_and_receive_command "foo" connection
                = Redis_common.Integer(42)
            );
            assert (
                Redis_common.Helpers.send_and_receive_command "foo" connection
                = Redis_common.Bulk(Redis_common.String("aaa"))
            );
        end
    in
    use_test_script
        [
            ReadThisLine("foo");
            WriteThisLine("+bar");

            ReadThisLine("foo");
            WriteThisLine("!"); (* Undecipherable *)

            ReadThisLine("foo");
            WriteThisLine(":42"); (* Integer reply *)

            ReadThisLine("foo");
            WriteThisLine("$3"); (* Bulk reply *)
            WriteThisLine("aaa")
        ]
        test_func;;

let test_send_and_receive_command_safely () =
    let test_func connection = 
        begin
            assert (
                Redis_common.Helpers.send_and_receive_command_safely "foo" connection
                = Redis_common.Status("bar")
            );
            try ignore (Redis_common.Helpers.send_and_receive_command_safely "foo" connection);
                assert(false) (* Should never reach this point *)
            with Redis_common.RedisServerError(x) ->
                assert(x = "Some error")
        end
    in
    use_test_script
        [
            ReadThisLine("foo");
            WriteThisLine("+bar");

            ReadThisLine("foo");
            WriteThisLine("-Some error")
        ]
        test_func;;

let test_send_with_value_and_receive_command_safely () =
    let test_func connection =
        begin
            assert (
                Redis_common.Helpers.send_with_value_and_receive_command_safely "foo" "bar" connection
                = Redis_common.Status("bar")
            );
            try ignore
                (Redis_common.Helpers.send_with_value_and_receive_command_safely
                    "foo" "bar" connection);
                assert(false) (* Should never reach this point *)
            with Redis_common.RedisServerError(x) ->
                assert(x = "Some error")
        end
    in
    use_test_script
        [
            ReadThisLine("foo 3");
            ReadThisLine("bar");
            WriteThisLine("+bar");
            ReadThisLine("foo 3");
            ReadThisLine("bar");
            WriteThisLine("-Some error")
        ]
        test_func;;

let test_aggregate_command  () =
    assert (
        "rory is cool" =
        (Redis_common.Helpers.aggregate_command "rory" ["is"; "cool"])
    );;

let test_send_multibulk_command () =
    let test_func connection =
        assert(Redis_common.Status("OK") = Redis_common.Helpers.send_multibulk_command ["rory"; "is"; "cool"] connection)
    in
    use_test_script
        [
            ReadThisLine("*3");
            ReadThisLine("$4");
            ReadThisLine("rory");
            ReadThisLine("$2");
            ReadThisLine("is");
            ReadThisLine("$4");
            ReadThisLine("cool");
            WriteThisLine("+OK");
        ]
        test_func;;
let test_send_multibulk_and_receive_command_safely () =
    (* Test for the "send_multibulk_and_receive_command_safely" function *)
    let test_func connection =
        begin
            assert (
                Redis_common.Helpers.send_multibulk_and_receive_command_safely ["foo"; "bar"] connection
                = Redis_common.Status("bar")
            );
            try ignore
                (Redis_common.Helpers.send_multibulk_and_receive_command_safely
                    ["foo"; "bar"] connection);
                assert(false) (* Should never reach this point *)
            with Redis_common.RedisServerError(x) ->
                assert(x = "Some error")
        end
    in
    use_test_script
        [
            ReadThisLine("*2");
            ReadThisLine("$3");
            ReadThisLine("foo");
            ReadThisLine("$3");
            ReadThisLine("bar");
            WriteThisLine("+bar");
            ReadThisLine("*2");
            ReadThisLine("$3");
            ReadThisLine("foo");
            ReadThisLine("$3");
            ReadThisLine("bar");
            WriteThisLine("-Some error")
        ]
        test_func;;

let test_handle_special_status () =
    Redis_common.Helpers.handle_special_status "rory is cool" (Redis_common.Status("rory is cool"));
    try Redis_common.Helpers.handle_special_status "OK" (Redis_common.Status("rory is cool"));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Received status(rory is cool)");
    try Redis_common.Helpers.handle_special_status "OK" (Redis_common.Error("rory is not cool"));
            failwith("Failed test")
        with Redis_common.RedisServerError(x) ->
            assert(x = "rory is not cool");
    try Redis_common.Helpers.handle_special_status "OK" (Redis_common.Undecipherable);
            failwith("Failed test")
        with Redis_common.RedisServerError(x) ->
            assert(x = "Could not decipher response");;

let test_handle_status () =
    Redis_common.Helpers.handle_status (Redis_common.Status("OK"));
    try Redis_common.Helpers.handle_status (Redis_common.Status("rory is cool"));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Received status(rory is cool)");;

let test_handle_integer_as_boolean () =
    assert (not (Redis_common.Helpers.handle_integer_as_boolean (Redis_common.Integer(0))));
    assert (Redis_common.Helpers.handle_integer_as_boolean (Redis_common.Integer(1)));
    try ignore (Redis_common.Helpers.handle_integer_as_boolean (Redis_common.Error("rory")));
            failwith("Failed test")
        with Redis_common.RedisServerError(x) ->
            assert(x = "rory");;

let test_handle_float () =
    assert (1.0 = (Redis_common.Helpers.handle_float (Redis_common.Bulk(Redis_common.String("1.0")))));
    (try ignore (Redis_common.Helpers.handle_float (Redis_common.Bulk(Redis_common.String("rory"))));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "\"rory\" is not a floating point number"));
    (try ignore (Redis_common.Helpers.handle_float (Redis_common.Integer(4)));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Did not recognize what I got back"));;

let test_expect_non_nil_multibulk () =
    assert([] = (Redis_common.Helpers.expect_non_nil_multibulk (Redis_common.Multibulk(Redis_common.MultibulkValue([])))));
    (try ignore (Redis_common.Helpers.expect_non_nil_multibulk (Redis_common.Multibulk(Redis_common.MultibulkNil)));
            (* Should never get here *)
            failwith("Failed test")
        with Redis_common.RedisNilError(x) ->
            assert(x = "Was not expecting MultibulkNil response."));;
