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
    let test_input_read, test_input_write = piped_channel()
    in
    let test_output_read, test_output_write = piped_channel()
    in
        begin
            begin
                output_string test_input_write "+bar\r\n";
                flush test_input_write;
                let result = Redis.send_and_receive_command "foo" (test_input_read, test_output_write)
                in
                begin
                    assert ( "foo\r" = (input_line test_output_read) );
                    assert ( Redis.Status("bar") = result )
                end
            end;
            begin
                output_string test_input_write "fail\r\n";
                flush test_input_write;
                let result = Redis.send_and_receive_command "foo" (test_input_read, test_output_write)
                in
                begin
                    assert ( "foo\r" = (input_line test_output_read) );
                    assert ( Redis.Error = result )
                end
            end
        end;;
