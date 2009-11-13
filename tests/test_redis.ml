let response_to_string r = match r with
    Redis.Status(x) -> Printf.sprintf "Status(%S)" x |
    Redis.Undecipherable -> "Undecipherable" |
    Redis.Integer(x) -> Printf.sprintf "Integer(%d)" x |
    Redis.Bulk(x) -> Printf.sprintf "Bulk(%S)" x;;

let test_read_string () =
    let test_pipe_read, test_pipe_write = Script.piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\n";
        flush test_pipe_write;
        assert (
            Redis.read_string test_pipe_read
            = "test string"
        )
    end;;

let test_send_text () =
    let test_func connection =
        Redis.send_text "foo" connection
    in
    Script.use_test_script [Script.ReadThisLine("foo\r")] test_func;;


let test_receive_answer () =
    let test_func connection =
        begin
            assert (
                Redis.receive_answer connection
                = Redis.Status("bar")
            );
            assert (
                Redis.receive_answer connection
                = Redis.Undecipherable
            );
            assert (
                Redis.receive_answer connection
                = Redis.Integer(42)
            );
            assert (
                Redis.receive_answer connection
                = Redis.Bulk("aaa")
            )
        end
    in
    Script.use_test_script 
        [
            Script.WriteThisLine("+bar\r");
            Script.WriteThisLine("!\r");
            Script.WriteThisLine(":42\r");
            Script.WriteThisLine("$3\r");
            Script.WriteThisLine("aaa\r")
        ]
        test_func;;

(*let test_send_and_receive_command () =
    let test_func connection =
        begin
            assert (
                Redis.send_and_receive_command "foo" connection
                = Redis.Status("bar")
            );
            assert (
                Redis.send_and_receive_command "foo" connection
                = Redis.Undecipherable
            );
            assert (
                Redis.send_and_receive_command "foo" connection
                = Redis.Integer(42)
            );
            assert (
                Redis.send_and_receive_command "foo" connection
                = Redis.Bulk("aaa")
            );
        end
    in
    Script.use_test_script "tests/scripts/reply_types.txt" test_func;;

(* Individual commands *)
 let test_ping () =
    let test_func connection =
        assert (
            (Redis.ping connection) = true
        )
    in
    Script.use_test_script "tests/scripts/ping.txt" test_func;;

 let test_exists () =
    let test_func connection = begin
        assert (
            (Redis.exists "real_key" connection) = true
        );
        assert (
            (Redis.exists "fake_key" connection) = false
        )
    end in
    Script.use_test_script "tests/scripts/exists.txt" test_func;;

 let test_set () =
    let test_func connection =
        Redis.set "key" "aaa" connection
    in
    Script.use_test_script "tests/scripts/set.txt" test_func;;

 let test_get () =
    let test_func connection = 
        assert (
            (Redis.get "key" connection) = "aaa"
        )
    in
    Script.use_test_script "tests/scripts/get.txt" test_func;;
*)
