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

let send_text text (_, out_chan) = begin
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
    (* Send command, and receive the results *)
    begin
        send_text command connection;
        receive_answer connection
    end;;


(* Individual commands *)

(* Connection handling *)
let ping connection =
    (* PING *)
    (send_and_receive_command "PING" connection) = Status("PONG");;

(* Commands operating on string values *)
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

let getset key new_value connection =
    (* GETSET *)
    send_text (Printf.sprintf "GETSET %s %d" key (String.length new_value)) connection;
    send_text new_value connection;
    match receive_answer connection with
        Bulk(x) -> x |
        _ -> failwith "Did not recognize what I got back";;

let mget keys connection = 
    (* MGET *)
    let joiner buf new_item = begin
            Buffer.add_char buf ' ';
            Buffer.add_string buf new_item
        end
    in
    let command_string = match keys with 
            [] -> failwith "Need at least one key"
            | x ->
                let out_buffer = Buffer.create 256
                in begin
                        Buffer.add_string out_buffer "MGET";
                        List.iter (joiner out_buffer) x;
                        out_buffer
                    end
    in
    match send_and_receive_command (Buffer.contents command_string) connection with
        Multibulk(l) -> l |
        _ -> failwith "Did not recognize what I got back";;
    
(* Commands operating on the key space *)
let exists key connection =
    (* EXISTS *)
    (send_and_receive_command ("EXISTS " ^ key) connection) = Integer(1);;

(* Multiple databases handling commands *)
let flushdb connection =
    (* FLUSHDB *)
    (send_and_receive_command "FLUSHDB" connection) = Status("OK");;
