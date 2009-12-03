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
    Script.use_test_script [Script.ReadThisLine("foo")] test_func;;

let test_receive_answer () =
    let test_func connection =
        begin
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Status("bar")
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
                Redis_util.receive_answer connection
                = Redis_util.Bulk(Redis_util.String("aaa"))
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Bulk(Redis_util.None)
            );
            assert (
                Redis_util.receive_answer connection
                = Redis_util.Multibulk([Redis_util.String("rory"); Redis_util.String("tim")])
            )
        end
    in
    Script.use_test_script 
        [
            Script.WriteThisLine("+bar"); (* Status *)
            Script.WriteThisLine("!"); (* Undecipherable *)
            Script.WriteThisLine(":42"); (* Integer *)
            Script.WriteThisLine("$3"); (* Bulk *)
            Script.WriteThisLine("aaa");
            Script.WriteThisLine("$-1"); (* None Bulk *)
            Script.WriteThisLine("*2"); (* Multibulk *)
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory");
            Script.WriteThisLine("$3");
            Script.WriteThisLine("tim")
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
    Script.use_test_script
        [
            Script.ReadThisLine("foo");
            Script.WriteThisLine("+bar");

            Script.ReadThisLine("foo");
            Script.WriteThisLine("!"); (* Undecipherable *)

            Script.ReadThisLine("foo");
            Script.WriteThisLine(":42"); (* Integer reply *)

            Script.ReadThisLine("foo");
            Script.WriteThisLine("$3"); (* Bulk reply *)
            Script.WriteThisLine("aaa")
        ]
        test_func;;

