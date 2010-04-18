(* Copyright (C) 2010 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Tests for "Remote server control commands" *)
open Redis

let test_info () =
    let test_func connection =
        let my_info = Redis.info connection
        in
        assert (
            "0" = Redis.Info.get my_info "uptime_in_days"
        );
        assert (
            List.mem "total_connections_received"
                (Redis.Info.get_fields my_info)
        );
    in
    Script.use_test_script
        [
            Script.ReadThisLine("INFO");
            Script.WriteThisLine("$229");
            Script.WriteThisLine("redis_version:0.07\r\nconnected_clients:1\r\nconnected_slaves:0\r\nused_memory:3187\r\nchanges_since_last_save:0\r\nlast_save_time:1237655729\r\ntotal_connections_received:1\r\ntotal_commands_processed:1\r\nuptime_in_seconds:25\r\nuptime_in_days:0")
        ]
        test_func;;

let test_slaveof () =
    let test_func connection =
        Redis.slaveof "other_redis_server" 6379 connection
    in
    Script.use_test_script
        [
            Script.ReadThisLine("SLAVEOF other_redis_server 6379");
            Script.WriteThisLine("+OK")
        ]
        test_func;;
