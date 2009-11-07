type next_script_line = ReadThisLine of string | WriteThisLine of string | NoMore | Skip;;
let next_script_line_to_string x =
    match x with
        ReadThisLine(body) -> Printf.sprintf "ReadThisLine(%S)" body | 
        WriteThisLine(body) -> Printf.sprintf "WriteThisLine(%S)" body | 
        Skip -> "Skip" |
        NoMore -> "NoMore";;

let get_next_line script_file =
    try
        let next_line = input_line script_file
        in
        let body = (String.sub next_line 1 ((String.length next_line) - 1)) ^ "\r"
        in
        match next_line.[0] with
            '<' -> ReadThisLine(body) |
            '>' -> WriteThisLine(body) |
            '#' -> Skip |
            _ -> failwith (Printf.sprintf "Error in script with line %S"  next_line)
    with End_of_file -> NoMore;;
    
let rec execute_each_line input output script_lines =
    let next_line = get_next_line script_lines
    in
    (*let _ = print_endline ("Next token: " ^ (next_script_line_to_string next_line)) in*)
    let go_down a_string = begin
            output_string input a_string;
            output_string input "\n";
            flush input
        end
    in
    match next_line with
        NoMore -> go_down "That's all folks!" |
        Skip -> execute_each_line input output script_lines |
        ReadThisLine(body) -> begin
                let inputted_line = input_line output
                in
                if not (body  = inputted_line)
                then failwith (Printf.sprintf "Inputted line %S does not match %S" body inputted_line);
                execute_each_line input output script_lines
            end |
        WriteThisLine(body) -> begin
                go_down body;
                execute_each_line input output script_lines
            end;;
        
let piped_channel () =
    let pipe_read, pipe_write = Unix.pipe()
    in
        (Unix.in_channel_of_descr pipe_read), (Unix.out_channel_of_descr pipe_write);; 

let use_test_script script_name test_function =
    let input_chan_read, input_chan_write = piped_channel()
    in 
    let output_chan_read, output_chan_write = piped_channel()
    in
    let _ = flush stdout (* Make sure we don't double print stdout buffer *)
    in
    let subprocess = Unix.fork()
    in
    match subprocess with
        0 -> (* Child process *)
            begin
                (test_function (input_chan_read, output_chan_write));
                let next_line = input_line input_chan_read in
                if not ((next_line) = "That's all folks!")
                then failwith (Printf.sprintf "Script %S did not finish properly (got %S)" script_name next_line)
                else exit 0
            end |
        x -> (* Parent process *)
            begin 
                execute_each_line input_chan_write output_chan_read (open_in script_name);
                match Unix.wait() with
                    (_, Unix.WEXITED(0)) -> () |
                    (_, _) -> exit 1
            end
