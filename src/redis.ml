let create_connection addr port =
    (* From a string of the address, and a port as an int, gets an input and output file discriptor *)
    let server = Unix.inet_addr_of_string addr
    in
    Unix.open_connection(
        Unix.ADDR_INET(server, port)
    )

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

let get_bulk_data size (in_chan, _) =
    let out_buf = Buffer.create size
    in begin
        Buffer.add_channel out_buf in_chan size;
        ignore (input_char in_chan); (* Remove \r\n *)
        ignore (input_char in_chan);
        Buffer.contents out_buf
    end;;

let send_text text (_, out_chan) = begin
        output_string out_chan text;
        output_string out_chan "\r\n";
        flush out_chan;
    end;;

type response = Status of string | Undecipherable | Integer of int | Bulk of string;;
let receive_answer connection =
    let in_chan, _ = connection
    in
    match (input_char in_chan) with
        '+' -> Status(read_string in_chan) |
        ':' -> Integer(int_of_string (read_string in_chan)) |
        '$' -> Bulk(
                get_bulk_data (int_of_string (read_string in_chan)) connection
            ) |
        _ -> begin
            ignore (input_line in_chan);
            Undecipherable
        end;;

let send_and_receive_command command connection =
    (* Send command, and recieve the results *)
    begin
        send_text command connection;
        receive_answer connection
    end;;


(* Individual commands *)
let ping connection =
    (* PING *)
    (send_and_receive_command "PING" connection) = Status("PONG");;

let exists key connection =
    (* EXISTS *)
    (send_and_receive_command ("EXISTS " ^ key) connection) = Integer(1);;

let set key value connection =
    (* SET *)
    begin
        send_text (Printf.sprintf "SET %s %d" key (String.length value)) connection;
        send_text value connection;
        match receive_answer connection with
            Status("OK") -> () |
            Status(x) -> failwith ("Received status(" ^ x ^ ") when setting " ^ key) |
            _ -> failwith "Did not recognize what I got back"
    end;;

let get key connection =
    (* GET *)
    match send_and_receive_command ("GET " ^ key) connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;
