(* Copyright (C) 2009 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

    Utility and protocol specific functions *)


let debug_string comment value  = begin
    Printf.printf "%s %S\n" comment value;
    flush stdout
end;;

let read_string in_chan =
    (* Read arbitratry length string (hopefully quite short) from current pos in in_chan until \r\n *)
    let rec iter out_buffer =
        match input_char in_chan with
            '\r' -> (
                match input_char in_chan with
                    '\n' -> Buffer.contents out_buffer |
                    x -> begin
                        Buffer.add_char out_buffer '\r';
                        Buffer.add_char out_buffer x;
                        iter out_buffer
                    end 
            ) |
            x -> begin
                Buffer.add_char out_buffer x;
                iter out_buffer
            end
    in
    iter (Buffer.create 100);;

let send_text text (_, out_chan) = begin
    (* Send the given text out to the connection *)
        output_string out_chan text;
        output_string out_chan "\r\n";
        flush out_chan;
    end;;

type bulk_data = Nil | String of string;;
type response = Status of string | Undecipherable | Integer of int | Bulk of bulk_data | Multibulk of bulk_data list | Error of string;;

let string_of_bulk_data bd =
    match bd with
        String(x) -> x |
        Nil -> failwith "Trying to extract string from none";;

let string_of_response r =
    let bulk_printer x =
        match x with
            Nil -> "Nil"
            | String(d) -> Printf.sprintf "String(%S)" d
    in
    let rec multi_bulk_list_to_string l =
        match l with
            [] -> ""
            | h :: t -> Printf.sprintf "; %s%s" (bulk_printer h) (multi_bulk_list_to_string t)
    in
    match r with
        Status(x) -> Printf.sprintf "Status(%S)" x |
        Undecipherable -> "Undecipherable" |
        Integer(x) -> Printf.sprintf "Integer(%d)" x |
        Bulk(x) -> Printf.sprintf "Bulk(%s)" (bulk_printer x) |
        Multibulk(x) -> match x with
            [] -> Printf.sprintf "Multibulk([])" |
            h :: t -> Printf.sprintf "Multibulk([%s%s])" (bulk_printer h) (multi_bulk_list_to_string t);;

let get_bulk_data (in_chan, _) =
    let size = int_of_string (read_string in_chan)
    in
    match size with 
        -1 -> Nil
        | 0 -> String("")
        | _ -> let out_buf = Buffer.create size
            in begin
                Buffer.add_channel out_buf in_chan size;
                ignore (input_char in_chan); (* Remove \r\n *)
                ignore (input_char in_chan);
                String(Buffer.contents out_buf)
            end;;

let get_multibulk_data size conn =
    let in_chan, out_chan = conn
    in
    let rec iter i data =
        if i == 0
        then Multibulk(List.rev data)
        else match input_char in_chan with
            '$' -> iter
                (i - 1)
                ((get_bulk_data conn) :: data)
            | _ -> Undecipherable
    in
    if size < 1
    then Multibulk([])
    else iter size [];;

let receive_answer connection =
    (* Get answer back from redis and cast it to the right type *)
    let in_chan, _ = connection
    in
    match (input_char in_chan) with
        '+' -> Status(read_string in_chan)
        | ':' -> Integer(int_of_string (read_string in_chan))
        | '$' -> Bulk(
                get_bulk_data connection
            )
        | '*' -> get_multibulk_data (int_of_string (read_string in_chan)) connection
        | '-' -> Error(read_string in_chan)
        | _ -> begin
            ignore (input_line in_chan);
            Undecipherable
        end;;

let send_and_receive_command command connection =
    (* Send command, and receive the result casted to the right type *)
    begin
        send_text command connection;
        receive_answer connection
    end;;

let aggregate_command command tokens = 
    let joiner buf new_item = begin
            Buffer.add_char buf ' ';
            Buffer.add_string buf new_item
        end
    in
    match tokens with 
        [] -> failwith "Need at least one key"
        | x ->
            let out_buffer = Buffer.create 256
            in begin
                    Buffer.add_string out_buffer command;
                    List.iter (joiner out_buffer) x;
                    Buffer.contents out_buffer
                end

let handle_status status =
    match status with
        Status("OK") -> () |
        Status(x) -> failwith ("Received status(" ^ x ^ ")") |
        Error(x) -> failwith ("Received error: " ^ x) |
        _ -> failwith "Did not recognize what I got back";;

