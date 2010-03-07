(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Test redis_util.ml *)

open Script
open Redis_util

let test_read_string () =
    let test_pipe_read, test_pipe_write = piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\n";
        flush test_pipe_write;
        assert (
            read_string test_pipe_read
            = "test string"
        )
    end;;

let test_send_text () =
    let test_func connection =
        send_text "foo" connection
    in
    use_test_script [ReadThisLine("foo")] test_func;;

let test_string_of_bulk_data () =
    begin
        assert ( "rory" = (string_of_bulk_data (String("rory"))));
        try
            (* Test failure when passing in Nil *)
            ignore (string_of_bulk_data (Nil))
        with Failure(_) -> ()
    end;;

let test_receive_answer () =
    let test_func connection =
        begin
            assert (
                receive_answer connection
                = Status("bar")
            );
            assert (
                receive_answer connection
                = Error("baz")
            );
            assert (
                receive_answer connection
                = Undecipherable
            );
            assert (
                receive_answer connection
                = Integer(42)
            );
            assert (
                match receive_answer connection with
                    BigInteger(x) ->
                        Big_int.eq_big_int x 
                            (Big_int.big_int_of_string "18446744073709551616")
                    | _ -> false
            );
            assert (
                receive_answer connection
                = Bulk(String("aaa"))
            );
            assert (
                receive_answer connection
                = Bulk(Nil)
            );
            assert (
                receive_answer connection
                = Multibulk([String("rory"); String("tim")])
            );
            assert (
                receive_answer connection
                = Multibulk([])
            )
        end
    in
    use_test_script 
        [
            WriteThisLine("+bar"); (* Status *)
            WriteThisLine("-baz"); (* Error *)
            WriteThisLine("!"); (* Undecipherable *)
            WriteThisLine(":42"); (* Integer *)
            WriteThisLine(":18446744073709551616"); (* BigInteger *)
            WriteThisLine("$3"); (* Bulk *)
            WriteThisLine("aaa");
            WriteThisLine("$-1"); (* Nil Bulk *)
            WriteThisLine("*2"); (* Multibulk *)
            WriteThisLine("$4");
            WriteThisLine("rory");
            WriteThisLine("$3");
            WriteThisLine("tim");
            WriteThisLine("*-1"); (* Empty Multibulk *)
        ]
        test_func;;

let test_send_and_receive_command () =
    let test_func connection =
        begin
            assert (
                send_and_receive_command "foo" connection
                = Status("bar")
            );
            assert (
                send_and_receive_command "foo" connection
                = Undecipherable
            );
            assert (
                send_and_receive_command "foo" connection
                = Integer(42)
            );
            assert (
                send_and_receive_command "foo" connection
                = Bulk(String("aaa"))
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

let test_aggregate_command  () =
    assert (
        "rory is cool" =
        (aggregate_command "rory" ["is"; "cool"])
    );;

let test_send_multibulk_command () =
    let test_func connection =
        assert(Redis_util.Status("OK") = send_multibulk_command ["rory"; "is"; "cool"] connection)
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

let test_handle_status () =
    handle_status (Status("OK"));
    try handle_status (Status("rory is cool"));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Received status(rory is cool)");
    try handle_status (Error("rory is not cool"));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Received error: rory is not cool");
    try handle_status (Undecipherable);
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Did not recognize what I got back")

let test_handle_integer () =
    assert (not (handle_integer (Integer(0))));
    assert (handle_integer (Integer(1)));
    try ignore (handle_integer (Undecipherable));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Did not recognize what I got back");;

let test_handle_float () =
    assert (1.0 = (handle_float (Bulk(String("1.0")))));
    (try ignore (handle_float (Bulk(String("rory"))));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "\"rory\" is not a floating point number"));
    (try ignore (handle_float (Integer(4)));
            failwith("Failed test")
        with Failure(x) ->
            assert(x = "Did not recognize what I got back"));;
