(* Utility and protocol specific functions *)

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

type bulk_data = Nil | Data of string;;
type response = Status of string | Undecipherable | Integer of int | Bulk of bulk_data | Multibulk of bulk_data list;;

let get_bulk_data (in_chan, _) =
    let size = int_of_string (read_string in_chan)
    in
    match size with 
        -1 -> Nil
        | 0 -> Data("")
        | _ -> let out_buf = Buffer.create size
            in begin
                Buffer.add_channel out_buf in_chan size;
                ignore (input_char in_chan); (* Remove \r\n *)
                ignore (input_char in_chan);
                Data(Buffer.contents out_buf)
            end;;

let get_multibulk_data size conn =
    let in_chan, out_chan = conn
    in
    let rec iter i data =
        match i with
            0 -> Multibulk(List.rev data)
            | x -> match input_char in_chan with
                '$' -> iter
                    (i - 1)
                    ((get_bulk_data conn) :: data)
                | _ -> Undecipherable
    in
    iter size [];;

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
