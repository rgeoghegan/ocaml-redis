let piped_channel () =
    let pipe_read, pipe_write = Unix.pipe()
    in
        (Unix.in_channel_of_descr pipe_read), (Unix.out_channel_of_descr pipe_write);; 

let test_read_string () =
    let test_pipe_read, test_pipe_write = piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\n";
        flush test_pipe_write;
        assert (
            Redis.read_string test_pipe_read
            = "test string"
        )
    end;;

let test_send_and_receive_command () =
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
                = Redis.Bulk(42)
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
