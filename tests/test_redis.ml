let response_to_string r =
    let bulk_printer x =
        match x with
            Redis.Nil -> "Nil"
            | Redis.Data(d) -> Printf.sprintf "Data(%S)" d
    in
    let rec multi_bulk_list_to_string l =
        match l with
            [] -> ""
            | h :: t -> Printf.sprintf "; %s%s" (bulk_printer h) (multi_bulk_list_to_string t)
    in
    match r with
        Redis.Status(x) -> Printf.sprintf "Status(%S)" x |
        Redis.Undecipherable -> "Undecipherable" |
        Redis.Integer(x) -> Printf.sprintf "Integer(%d)" x |
        Redis.Bulk(x) -> Printf.sprintf "Bulk(%s)" (bulk_printer x) |
        Redis.Multibulk(x) -> match x with
            [] -> Printf.sprintf "Multibulk([])" |
            h :: t -> Printf.sprintf "Multibulk([%s%s])" (bulk_printer h) (multi_bulk_list_to_string t);;

let test_read_string () =
    let test_pipe_read, test_pipe_write = Script.piped_channel()
    in
    begin
        output_string test_pipe_write "test string\r\n";
        flush test_pipe_write;
        assert (
            Redis.read_string test_pipe_read
            = "test string"
        )
    end;;

let test_send_text () =
    let test_func connection =
        Redis.send_text "foo" connection
    in
    Script.use_test_script [Script.ReadThisLine("foo")] test_func;;


let test_receive_answer () =
    let test_func connection =
        begin
            assert (
                Redis.receive_answer connection
                = Redis.Status("bar")
            );
            assert (
                Redis.receive_answer connection
                = Redis.Undecipherable
            );
            assert (
                Redis.receive_answer connection
                = Redis.Integer(42)
            );
            assert (
                Redis.receive_answer connection
                = Redis.Bulk(Redis.Data("aaa"))
            );
            assert (
                Redis.receive_answer connection
                = Redis.Bulk(Redis.Nil)
            );
            assert (
                Redis.receive_answer connection
                = Redis.Multibulk([Redis.Data("rory"); Redis.Data("tim")])
            )
        end
    in
    Script.use_test_script 
        [
            Script.WriteThisLine("+bar"); (* Status *)
            Script.WriteThisLine("!"); (* Undecipherable *)
            Script.WriteThisLine(":42"); (* Integer *)
            Script.WriteThisLine("$3"); (* Bulk *)
            Script.WriteThisLine("aaa");
            Script.WriteThisLine("$-1"); (* Nil Bulk *)
            Script.WriteThisLine("*2"); (* Multibulk *)
            Script.WriteThisLine("$4");
            Script.WriteThisLine("rory");
            Script.WriteThisLine("$3");
            Script.WriteThisLine("tim")
        ]
        test_func;;

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
                = Redis.Bulk(Redis.Data("aaa"))
            );
        end
    in
    Script.use_test_script
        [
            Script.ReadThisLine("foo");
            Script.WriteThisLine("+bar");

            Script.ReadThisLine("foo");
            Script.WriteThisLine("!"); (* Undecipherable *)

            Script.ReadThisLine("foo");
            Script.WriteThisLine(":42"); (* Integer reply *)

            Script.ReadThisLine("foo");
            Script.WriteThisLine("$3"); (* Bulk reply *)
            Script.WriteThisLine("aaa")
        ]
        test_func;;

(* Individual commands *)
let test_ping () =
    let test_func connection =
        assert (
            (Redis.ping connection) = true
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("PING");
            Script.WriteThisLine("+PONG")
        ]
        test_func;;

let test_exists () =
    let test_func connection = begin
        assert (
            (Redis.exists "real_key" connection) = true
        );
        assert (
            (Redis.exists "fake_key" connection) = false
        )
    end in
    Script.use_test_script
        [
            Script.ReadThisLine("EXISTS real_key");
            Script.WriteThisLine(":1");
            Script.ReadThisLine("EXISTS fake_key");
            Script.WriteThisLine(":0")
        ]
        test_func;;

let test_set () =
    let test_func connection =
        Redis.set "key" "aaa" connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SET key 3");
            Script.ReadThisLine("aaa");
            Script.WriteThisLine("+OK")
        ]
        test_func;;

let test_get () =
    let test_func connection = 
        assert (
            (Redis.get "key" connection) = Redis.Data("aaa")
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("GET key");
            Script.WriteThisLine("$3");
            Script.WriteThisLine("aaa")
        ]
        test_func;;

let test_getset () =
    let test_func connection =
        assert (
            (Redis.getset "key" "now" connection) = Redis.Data("previous")
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("GETSET key 3");
            Script.ReadThisLine("now");
            Script.WriteThisLine("$8");
            Script.WriteThisLine("previous")
        ]
        test_func;;

let test_mget () =
    let test_func conn =
        assert (
            (Redis.mget ["rory"; "tim"] conn) = [Redis.Data("cool"); Redis.Data("not cool")]
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("MGET rory tim");
            Script.WriteThisLine("*2");
            Script.WriteThisLine("$4");
            Script.WriteThisLine("cool");
            Script.WriteThisLine("$8");
            Script.WriteThisLine("not cool")
        ]
        test_func;;

let test_flushdb () =
    let test_func conn =
        assert (
            true = Redis.flushdb conn
        )
    in
    Script.use_test_script
        [
            Script.ReadThisLine("FLUSHDB");
            Script.WriteThisLine("+OK")
        ]
        test_func;;
