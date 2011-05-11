(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Simple smoke test to be run on local server. *)

open OUnit

let suite = 
  let setup () = 
    let conn = Redis.create_connection () in
    Redis.auth conn "qwerty";
    Redis.flushall conn;
    conn
  and teardown conn = 
    Redis.flushall conn;
    Redis.quit conn
  and one x = Some x in
  let test f = (bracket setup f teardown) in
  "smoke test" >:::
    [
      "general" >:: 
        (test 
           (fun conn ->
             assert_equal false (Redis.exists conn "rory");
             Redis.set conn "rory" "cool";
             assert_equal (one "cool") (Redis.get conn "rory");
             assert_equal (one "cool") (Redis.getset conn "rory" "not cool");
             assert_equal [Some "not cool"; None] (Redis.mget conn ["rory"; "tim"]);

             assert_equal false (Redis.setnx conn "rory" "uncool");
             assert_equal (one "not cool") (Redis.get conn "rory");
             Redis.mset conn [("rory", "cool"); ("tim", "not cool")];
             assert_bool "" (not (Redis.msetnx conn [("rory", "not cool"); ("tim", "cool")]));

             Redis.set conn "rory" "0";
             assert_equal 1L (Redis.incr conn "rory");
             assert_equal 5L (Redis.incrby conn "rory" 4);
             assert_equal 4L (Redis.decr conn "rory");
             assert_equal 2L (Redis.decrby conn "rory" 2);

             Redis.set conn "rory" "very ";
             assert_equal 9 (Redis.append conn "rory" "cool");

             assert_equal "cool" (Redis.getrange conn "rory" 5 9);

             assert_equal 2 (Redis.del conn ["rory"; "tim"]);
             assert_equal false (Redis.del_one conn "rory");
    
             Redis.set conn "rory" "cool";
             assert_equal Redis.Value.Nil (Redis.value_type conn "tim");
             assert_equal Redis.Value.String (Redis.value_type conn "rory");

             assert_equal ["rory"] (Redis.keys conn "*"); 
             assert_equal (one "rory") (Redis.randomkey conn);
             Redis.rename conn "rory" "tim";

             assert_equal (one "tim") (Redis.randomkey conn);

             Redis.set conn "rory" "more cool";
             assert_equal false (Redis.renamenx conn "rory" "tim");

             Redis.setex conn "rory" 10 "cool";
             
             assert_equal 2 (Redis.dbsize conn );

             Redis.set conn "rory" "cool";
             assert_bool "" (Redis.expire conn "rory" 10);
             assert_bool "" (10 >= Redis.ttl conn "rory");

             assert_bool "" (Redis.expireat conn "tim" (Unix.time() +. 10.));
             assert_bool "" (10 >= Redis.ttl conn "tim");
           ));

      "list operations" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del conn ["rory"]);
             assert_equal 1 (Redis.rpush conn "rory" "cool");
             assert_equal 2 (Redis.lpush conn "rory" "even cooler");
             assert_equal 2 (Redis.llen conn "rory");
             assert_equal ["even cooler"; "cool"] (Redis.lrange conn "rory" 0 1);

             Redis.ltrim conn "rory" 0 0;
             assert_equal (one "even cooler") (Redis.lindex conn "rory" 0);
             Redis.lset conn "rory" 0 "just cool";
             ignore (Redis.rpush conn "rory" "cool");
             assert_equal 1 (Redis.lrem conn "rory" 0 "cool");
             
             ignore (Redis.rpush conn "rory" "cool");
             ignore (Redis.rpush conn "rory" "even cooler");
             
             assert_equal (one "just cool") (Redis.lpop conn "rory");
             assert_equal (one "even cooler") (Redis.rpop conn "rory");
             
             ignore (Redis.rpush conn "cool" "rory");
             ignore (Redis.rpush conn "cool" "tim");
             assert_equal (one "tim") (Redis.rpoplpush conn "cool" "not_cool");
           ));

      "set operations" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del_one conn "tim");
             assert_bool "" (Redis.sadd conn "tim" "not cool");
             assert_bool "" (Redis.sadd conn "tim" "smells");
             assert_bool "" (Redis.srem conn "tim" "smells");
             assert_equal (one "not cool") (Redis.spop conn "tim");

             ignore ( Redis.del_one conn "rory");
             assert_bool "" (Redis.sadd conn "rory" "cool");
             assert_bool "" ( Redis.sadd conn "tim" "even cooler");
             assert_bool "" ( Redis.smove conn "tim" "rory" "even cooler");
    
             assert_equal 2 (Redis.scard conn "rory");
             assert_bool "" ( Redis.sismember conn "rory" "cool");

             ignore (Redis.srem conn "rory" "cool");
             assert_equal "even cooler" (List.hd (Redis.smembers conn "rory"));
             
             ignore (Redis.sadd conn "tim" "even cooler");
             
             assert_equal "even cooler" (List.hd (Redis.sinter conn ["rory"; "tim"]));
             
             assert_equal 1 (Redis.sinterstore conn "bob" ["rory"; "tim"]);
             
             assert_equal "even cooler" (List.hd (Redis.sunion conn ["rory"; "tim"]));
             assert_equal 1 (Redis.sunionstore conn "bob" ["rory"; "tim"]);
             
             ignore ( Redis.srem conn "tim" "even cooler");
             assert_equal "even cooler" (List.hd (Redis.sdiff conn "rory" ["tim"]));
             assert_equal 1 (Redis.sdiffstore conn "bob" "rory" ["tim"]);
             
             ignore (Redis.del conn ["rory"; "tim"]);
             ignore (Redis.sadd conn "rory" "cool");
             assert_equal (one "cool") (Redis.srandmember conn "rory");
             assert_equal None (Redis.srandmember conn "non_existent_key");
           ));
        
      "multiple databases" >:: 
        (test 
           (fun conn ->
             Redis.select conn 1;
             Redis.select conn 0;

             ignore (Redis.lpush conn "rory" "1");
             ignore (Redis.lpush conn "rory" "2");
             ignore (Redis.lpush conn "rory" "11");
             
             assert_bool "" (Redis.move conn "rory" 1);
           ));

      "sorted sets" >:: 
        (test 
           (fun conn ->
             assert_bool "" (Redis.zadd conn "coolest" 42.0 "rory");
             assert_bool "" (Redis.zrem conn "coolest" "rory");

             ignore (Redis.zadd conn "coolest" 1.0 "rory");
             ignore (Redis.zadd conn "coolest" 99.0 "tim");

             assert_equal ["rory"; "tim"] (Redis.zrange conn "coolest" 0 1);
             
             let l = Redis.zrange_with_scores conn "coolest" 0 1 in
             assert_equal [("rory", 1.0); ("tim", 99.0)] l;

             assert_equal ["tim"; "rory"] (Redis.zrevrange conn "coolest" 0 1);
             
             let l = Redis.zrevrange_with_scores conn "coolest" 0 1 in
             assert_equal [("tim", 99.0); ("rory", 1.0)] l;
             
             assert_equal "rory" (List.hd (Redis.zrangebyscore conn
                                             "coolest" 0.0 100.0
                                             ~limit:(Redis.Limit (0, 1))));
             
             assert_equal 2.0 (Redis.zincrby conn "coolest" 1.0 "rory");

             assert_equal None (Redis.zrank conn "coolest" "joel");
             assert_equal (Some 0) (Redis.zrank conn "coolest" "rory");
             assert_equal (Some 1) (Redis.zrevrank conn "coolest" "rory");

             assert_equal 2 (Redis.zcard conn "coolest");
             assert_equal (Some 2.0) (Redis.zscore conn "coolest" "rory");
             assert_equal 1 (Redis.zremrangebyrank conn "coolest" 0 0);
             assert_equal 1 (Redis.zremrangebyscore conn "coolest" 80.0 120.0);
             
             ignore (Redis.del_one conn "tim");
             ignore (Redis.del_one conn "rory");
             ignore (Redis.zadd conn "rory" 10.0 "cool");
             ignore (Redis.zadd conn "tim" 20.0 "uncool");

             assert_equal 2 (Redis.zunionstore conn "union" ["rory"; "tim"]);
             assert_equal 2 (Redis.zunionstore conn "union" ["rory"; "tim"] ~aggregate:Redis.Sum);
             assert_equal 2 (Redis.zunionstore conn "union" ["rory"; "tim"] ~aggregate:Redis.Min);
             assert_equal 2 (Redis.zunionstore conn "union" ["rory"; "tim"] ~aggregate:Redis.Max);
             
             assert_equal 2 (Redis.zunionstore_with_weights conn "union" ["rory"; "tim"] [1.0; 0.5]);
             assert_equal 2 (Redis.zunionstore_with_weights conn "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Sum);
             assert_equal 2 (Redis.zunionstore_with_weights conn "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Min);
             assert_equal 2 (Redis.zunionstore_with_weights conn "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Max);
             
             assert_equal 0 (Redis.zinterstore conn "inter" ["rory"; "tim"]);
             assert_equal 0 (Redis.zinterstore conn "inter" ["rory"; "tim"] ~aggregate:Redis.Sum);
             assert_equal 0 (Redis.zinterstore conn "inter" ["rory"; "tim"] ~aggregate:Redis.Min);
             assert_equal 0 (Redis.zinterstore conn "inter" ["rory"; "tim"] ~aggregate:Redis.Max);
             
             assert_equal 0 (Redis.zinterstore_with_weights conn "inter" ["rory"; "tim"] [1.0; 0.5]);
             assert_equal 0 (Redis.zinterstore_with_weights conn "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Sum);
             assert_equal 0 (Redis.zinterstore_with_weights conn "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Min);
             assert_equal 0 (Redis.zinterstore_with_weights conn "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Max);
           ));

      "sort" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del_one conn "rory");
             ignore (Redis.lpush conn "rory" "1");
             ignore (Redis.lpush conn "rory" "2");

             assert_equal "2" (List.hd (Redis.sort conn "rory" ~alpha:Redis.Alpha ~order:Redis.Desc));
             assert_equal "2" (List.hd (Redis.sort conn "rory" ~pattern:Redis.NoSort));
           ));

      "complex sort" >:: 
        (test 
           (fun conn ->
             (* This requires quite some test data to set up *)
             let fields = ["name"; "yob"] in
             let data = [["Rory"; "1984"]; ["Bob"; "1980"]] in
             let add_record index record =
               let add_field name value =
                 Redis.set conn (name ^ "_" ^ (string_of_int index)) value
               in
               ignore (Redis.rpush conn "people" (string_of_int index));
               List.iter2 add_field fields record;
               index + 1
             in
             ignore (List.fold_left add_record 1 data);
             ignore (Redis.hset conn "hash_1" "yob" "1984");
             ignore (Redis.hset conn "hash_2" "yob" "1980");

             let l = Redis.sort conn "people"~get:(Redis.KeyPattern("name_*")) in
             assert_equal ["Rory"; "Bob"] l;

             let l = Redis.sort conn "people" ~get:(Redis.FieldPattern("hash_*", "yob")) in
             assert_equal ["1984"; "1980"] l;

             let l = Redis.sort_get_many conn "people" ["name_*"; "yob_*"] 
               ~pattern:(Redis.KeyPattern("yob_*")) in
             assert_equal [["Bob"; "1980"]; ["Rory"; "1984"]] l;
             
             let l = Redis.sort_get_many conn "people" ["name_*"; "yob_*"] 
               ~pattern:(Redis.FieldPattern("hash_*", "yob")) in
             assert_equal [["Bob"; "1980"]; ["Rory"; "1984"]] l;

             assert_equal 2 (Redis.sort_and_store conn "people" ["name_*"] "results" 
                               ~pattern:(Redis.KeyPattern("yob_*")));
           ));

      "hashes" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del conn ["rory"]);
             assert_bool "" (Redis.hset conn "rory" "cool" "true");
             assert_bool "" (not (Redis.hset conn "rory" "cool" "false"));

             ignore (Redis.del conn ["rory"]);
             assert_bool "" (Redis.hsetnx conn "rory" "cool" "true");
             assert_bool "" (not (Redis.hsetnx conn "rory" "cool" "false"));
             assert_equal (one "true") (Redis.hget conn "rory" "cool");

             assert_bool "" (Redis.hdel conn "rory" "cool");
             assert_bool "" (not (Redis.hdel conn "rory" "cool"));
             
             assert_bool "" (Redis.hset conn "rory" "handsome" "true");
             assert_equal (one "true") (Redis.hget conn "rory" "handsome");
             assert_equal None (Redis.hget conn "rory" "boring");
             
             assert_equal [Some "true"; None] (Redis.hmget conn "rory" ["handsome"; "boring"]);
             Redis.hmset conn "rory" [("handsome", "false"); ("boring", "true")];

             assert_equal 26L (Redis.hincrby conn "rory" "age" 26);

             assert_bool "" (Redis.hexists conn "rory" "handsome");
             assert_bool "" (not (Redis.hexists conn "rory" "andsome"));
    
             assert_equal 3 (Redis.hlen conn "rory");
             
             ignore (Redis.del conn ["rory"]);
             ignore (Redis.hset conn "rory" "cool" "true");

             assert_equal ["cool"] (Redis.hkeys conn "rory");
             assert_equal ["true"] (Redis.hvals conn "rory");
             assert_equal [("cool", "true")] (Redis.hgetall conn "rory");
           ));

      "remote control" >:: 
        (test 
           (fun conn ->
             assert_equal "master" (Redis.Info.get (Redis.info conn) "role");
           ));
      
      (* These tests take a noticeable amount of time, so it's easier to slot them at the end *)

      "long running" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del conn ["rory"; "tim"; "bob"]);
             ignore (Redis.rpush conn "rory" "cool");

             assert_equal (one ("rory", "cool")) (Redis.blpop conn "rory");

             assert_equal None (Redis.blpop conn "rory" ~timeout:(Redis.Seconds 1));
             ignore (Redis.rpush conn "tim" "not cool");
             assert_equal (one ("tim", "not cool")) (Redis.blpop_many conn ["rory"; "tim"]);
             assert_equal None (Redis.blpop_many conn ["rory"; "tim"] ~timeout:(Redis.Seconds 1));

             ignore (Redis.rpush conn "rory" "cool");
             assert_equal (one ("rory", "cool")) (Redis.brpop conn "rory");
             assert_equal None (Redis.brpop conn "rory" ~timeout:(Redis.Seconds 1));

             ignore (Redis.rpush conn "tim" "not cool");
             assert_equal (one ("tim", "not cool")) (Redis.blpop_many conn ["rory"; "tim"; "bob"]);
             assert_equal None (Redis.blpop_many conn ["rory"; "tim"; "bob"] ~timeout:(Redis.Seconds 1));
           ));

      "persistence" >:: 
        (test 
           (fun conn ->
             Redis.save conn;
             Redis.bgsave conn;

             assert_bool "" (0L < Redis.lastsave conn);
             Redis.bgrewriteaof conn;
           ));
        ]

let shutdown_suite = 
  let setup () : Redis.Connection.t = 
    let conn = Redis.create_connection () in
    Redis.auth conn "qwerty";
    Redis.flushall conn;
    conn
  in
  let teardown (conn : Redis.Connection.t) = 
    Redis.flushall conn;
    Redis.quit conn
  in
  let test f = (bracket setup f teardown) in
  "smoke test" >:::
    [
      "general" >:: 
        (test 
           (fun conn ->
             (* Shutdown is different from quit, so it needs it's own test function *)
             Redis.auth conn "qwerty";
             Redis.shutdown conn;
           ));
    ]
