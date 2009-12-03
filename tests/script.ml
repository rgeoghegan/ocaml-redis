(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   This module scripts simple queries and responses from a fake redis server for testing. *)

let response_to_string r =
    let bulk_printer x =
        match x with
            Redis_util.None -> "None"
            | Redis_util.String(d) -> Printf.sprintf "String(%S)" d
    in
    let rec multi_bulk_list_to_string l =
        match l with
            [] -> ""
            | h :: t -> Printf.sprintf "; %s%s" (bulk_printer h) (multi_bulk_list_to_string t)
    in
    match r with
        Redis_util.Status(x) -> Printf.sprintf "Status(%S)" x |
        Redis_util.Undecipherable -> "Undecipherable" |
        Redis_util.Integer(x) -> Printf.sprintf "Integer(%d)" x |
        Redis_util.Bulk(x) -> Printf.sprintf "Bulk(%s)" (bulk_printer x) |
        Redis_util.Multibulk(x) -> match x with
            [] -> Printf.sprintf "Multibulk([])" |
            h :: t -> Printf.sprintf "Multibulk([%s%s])" (bulk_printer h) (multi_bulk_list_to_string t);;

let debug_string comment value  = begin
    Printf.printf "%s %S\n" comment value;
    flush stdout
end;;

type next_script_line = ReadThisLine of string | WriteThisLine of string;;
let next_script_line_to_string x =
    match x with
        ReadThisLine(body) -> Printf.sprintf "ReadThisLine(%S)" body 
        | WriteThisLine(body) -> Printf.sprintf "WriteThisLine(%S)" body
;;

let rec execute_each_line input output script =
    let send_out a_string = begin
            output_string input a_string;
            output_string input "\r\n";
            flush input;
        end
    in
    match script with
        [] ->
                send_out "That's all folks!"
        | h :: tail -> 
                (* let _ = debug_string "Next token: " (next_script_line_to_string h) in *)
                let _ = match h with
                    ReadThisLine(body) ->
                        let inputted_line = input_line output
                        in
                        if  not ((body ^ "\r") = inputted_line)
                            then failwith (Printf.sprintf "Inputted line %S does not match %S" body inputted_line)
                    | WriteThisLine(body) -> send_out body
                in
                execute_each_line input output tail
;;

let piped_channel () =
    let pipe_read, pipe_write = Unix.pipe()
    in
        (Unix.in_channel_of_descr pipe_read), (Unix.out_channel_of_descr pipe_write);; 

let use_test_script script test_function =
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
                let next_line = input_line input_chan_read
                in
                if not ((next_line) = "That's all folks!\r")
                    then failwith (Printf.sprintf "Script did not finish properly (got %S)" next_line)
                    else exit 0
            end |
        x -> (* Parent process *)
            begin 
                execute_each_line input_chan_write output_chan_read script;
                match Unix.wait() with
                    (_, Unix.WEXITED(0)) -> () |
                    (_, _) -> exit 1
            end;;
