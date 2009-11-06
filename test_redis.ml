let test_read_string () =
    let test_pipe_in, test_pipe_out = Unix.pipe()
    in
    let test_out_channel = Unix.out_channel_of_descr test_pipe_out
    in
    begin
        output_string test_out_channel "test string\r\n";
        flush test_out_channel;
        assert (
            Redis.read_string (Unix.in_channel_of_descr test_pipe_in)
            = "test string"
        )
    end;;

(*
let _ =
    begin
        Printf.printf "Running test_read_string... ";
        test_read_string();
        Printf.printf "passed\n"
    end;;
*)
