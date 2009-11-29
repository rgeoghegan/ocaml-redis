open Redis_util

(* Connection handling *)
let ping connection =
    (* PING *)
    (send_and_receive_command "PING" connection) = Status("PONG");;

let create_connection addr port =
    (* From a string of the address, and a port as an int, gets an input and output file discriptor *)
    let server = Unix.inet_addr_of_string addr
    in
    Unix.open_connection(
        Unix.ADDR_INET(server, port)
    )

(* Individual commands *)

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

let setnx key value connection =
    (* SETNX *)
    begin
        send_text (Printf.sprintf "SETNX %s %d" key (String.length value)) connection;
        send_text value connection;
        match receive_answer connection with
            Status("OK") -> () |
            Status(x) -> failwith ("Received status(" ^ x ^ ") when setting " ^ key) |
            _ -> failwith "Did not recognize what I got back"
    end;;
    
    
(* Commands operating on the key space *)
let exists key connection =
    (* EXISTS *)
    (send_and_receive_command ("EXISTS " ^ key) connection) = Integer(1);;

(* Multiple databases handling commands *)
let flushdb connection =
    (* FLUSHDB *)
    (send_and_receive_command "FLUSHDB" connection) = Status("OK");;
