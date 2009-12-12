(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Test redis_util.ml *)

open Script

let test_read_string () =
    let test_pipe_read, test_pipe_write = piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\n";
        flush test_pipe_write;
        assert (
            Redis_util.read_string test_pipe_read
            = "test string"
        )
    end;;

let test_send_text () =
    let test_func connection =
        Redis_util.send_text "foo" connection
    in
    use_test_script [ReadThisLine("foo")] test_func;;

let test_string_of_bulk_data () =
    begin
        assert ( "rory" = (Redis_util.string_of_bulk_data (Redis_util.String("rory"))));
        try
            (* Test failure when passing in Nil *)
            ignore (Redis_util.string_of_bulk_data (Redis_util.Nil))
        with Failure(_) -> ()
    end;;

let test_receive_answer () =
    let test_func connection =
        begin
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Status("bar")
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Error("baz")
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Undecipherable
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Integer(42)
            );
            assert (
                match Redis_util.receive_answer connection with
                    Redis_util.BigInteger(x) ->
                        Big_int.eq_big_int x 
                            (Big_int.big_int_of_string "100020003000")
                    | _ -> false
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Bulk(Redis_util.String("aaa"))
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Bulk(Redis_util.Nil)
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Multibulk([Redis_util.String("rory"); Redis_util.String("tim")])
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Multibulk([])
            )
        end
    in
    use_test_script 
        [
            WriteThisLine("+bar"); (* Status *)
            WriteThisLine("-baz"); (* Error *)
            WriteThisLine("!"); (* Undecipherable *)
            WriteThisLine(":42"); (* Integer *)
            WriteThisLine(":100020003000"); (* BigInteger *)
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
                Redis_util.send_and_receive_command "foo" connection
                = Redis_util.Status("bar")
            );
            assert (
                Redis_util.send_and_receive_command "foo" connection
                = Redis_util.Undecipherable
            );
            assert (
                Redis_util.send_and_receive_command "foo" connection
                = Redis_util.Integer(42)
            );
            assert (
                Redis_util.send_and_receive_command "foo" connection
                = Redis_util.Bulk(Redis_util.String("aaa"))
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

