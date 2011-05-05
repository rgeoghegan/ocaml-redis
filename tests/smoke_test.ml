(* Copyright (C) 2011 Rory Geoghegan - r.geoghegan@gmail.com
   Released under the BSD license. See the LICENSE.txt file for more info.

   Simple smoke test to be run on local server. *)

open OUnit

let suite = 
  let setup () = 
    let conn = Redis.create_connection () in
    Redis.auth "qwerty" conn;
    Redis.flushall conn;
    conn
  in
  let teardown conn = 
    Redis.flushall conn;
    Redis.quit conn
  in
  let test f = (bracket setup f teardown) in
  "smoke test" >:::
    [
      "general" >:: 
        (test 
           (fun conn ->
             assert_equal false (Redis.exists "rory" conn);
             Redis.set "rory" "cool" conn;
             assert_equal "cool" (Redis.string_of_bulk_data (Redis.get "rory" conn));
             assert_equal "cool" (Redis.string_of_bulk_data (Redis.getset "rory" "not cool" conn));
             assert_equal [Redis.String("not cool"); Redis.Nil] (Redis.mget ["rory"; "tim"] conn);
             assert_equal false (Redis.setnx "rory" "uncool" conn);
             assert_equal "not cool" (Redis.string_of_bulk_data (Redis.get "rory" conn));

             Redis.mset [("rory", "cool"); ("tim", "not cool")] conn;
             assert_bool "" (not (Redis.msetnx [("rory", "not cool"); ("tim", "cool")] conn));

             Redis.set "rory" "0" conn;
             assert_equal 1 (Redis.incr "rory" conn);
             assert_equal 5 (Redis.incrby "rory" 4 conn);
             assert_equal 4 (Redis.decr "rory" conn);
             assert_equal 2 (Redis.decrby "rory" 2 conn);

             Redis.set "rory" "very " conn;
             assert_equal 9 (Redis.append "rory" "cool" conn);

             assert_equal "cool" (Redis.string_of_bulk_data (Redis.substr "rory" 5 9 conn));

             assert_equal 2 (Redis.del ["rory"; "tim"] conn);
             assert_equal false (Redis.del_one "rory" conn);
    
             Redis.set "rory" "cool" conn;
             assert_equal Redis.RedisNil (Redis.value_type "tim" conn);
             assert_equal Redis.RedisString (Redis.value_type "rory" conn);

             assert_equal ["rory"] (Redis.keys "*" conn); 
             assert_equal "rory" (Redis.randomkey conn);
             Redis.rename "rory" "tim" conn;

             assert_equal "tim" (Redis.randomkey conn);

             Redis.set "rory" "more cool" conn;
             assert_equal false (Redis.renamenx "rory" "tim" conn);

             Redis.setex "rory" 10 "cool" conn;
             
             assert_equal 2 (Redis.dbsize conn );

             Redis.set "rory" "cool" conn;
             assert_bool "" (Redis.expire "rory" 10 conn);
             assert_bool "" (10 >= Redis.ttl "rory" conn);

             assert_bool "" (Redis.expireat "tim" (Unix.time() +. 10.) conn);
             assert_bool "" (10 >= Redis.ttl "tim" conn);
           ));
      "list operations" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del ["rory"] conn);
             assert_equal 1 (Redis.rpush "rory" "cool" conn);
             assert_equal 2 (Redis.lpush "rory" "even cooler" conn);
             assert_equal 2 (Redis.llen "rory" conn);
             assert_equal [Redis.String("even cooler"); Redis.String("cool")]
               (Redis.lrange "rory" 0 1 conn);

             Redis.ltrim "rory" 0 0 conn;
             assert_equal "even cooler" (Redis.string_of_bulk_data (Redis.lindex "rory" 0 conn));
             Redis.lset "rory" 0 "just cool" conn;
             ignore (Redis.rpush "rory" "cool" conn);
             assert_equal 1 (Redis.lrem "rory" 0 "cool" conn);
             
             ignore (Redis.rpush "rory" "cool" conn);
             ignore (Redis.rpush "rory" "even cooler" conn);
             
             assert_equal "just cool" (Redis.string_of_bulk_data (Redis.lpop "rory" conn));
             assert_equal "even cooler" (Redis.string_of_bulk_data (Redis.rpop "rory" conn));
             
             ignore (Redis.rpush "cool" "rory" conn);
             ignore (Redis.rpush "cool" "tim" conn);
             assert_equal "tim" (Redis.string_of_bulk_data (Redis.rpoplpush "cool" "not_cool" conn));
           ));

      "set operations" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del_one "tim" conn);
             assert_bool "" (Redis.sadd "tim" "not cool" conn);
             assert_bool "" (Redis.sadd "tim" "smells" conn);
             assert_bool "" (Redis.srem "tim" "smells" conn);
             assert_equal "not cool" (Redis.string_of_bulk_data (Redis.spop "tim" conn));

             ignore ( Redis.del_one "rory" conn);
             assert_bool "" (Redis.sadd "rory" "cool" conn);
             assert_bool "" ( Redis.sadd "tim" "even cooler" conn);
             assert_bool "" ( Redis.smove "tim" "rory" "even cooler" conn );
    
             assert_equal 2 (Redis.scard "rory" conn );
             assert_bool "" ( Redis.sismember "rory" "cool" conn );

             ignore ( Redis.srem "rory" "cool" conn );
             assert_equal "even cooler" (Redis.string_of_bulk_data (List.hd (Redis.smembers "rory" conn)) );
             
             ignore (Redis.sadd "tim" "even cooler" conn);
             
             assert_equal "even cooler" (Redis.string_of_bulk_data (List.hd (Redis.sinter ["rory"; "tim"] conn)) );
             
             assert_equal 1 ( Redis.sinterstore "bob" ["rory"; "tim"] conn );
             
             assert_equal "even cooler" (Redis.string_of_bulk_data (List.hd (Redis.sunion ["rory"; "tim"] conn)) );
             assert_equal 1 (Redis.sunionstore "bob" ["rory"; "tim"] conn );
             
             ignore ( Redis.srem "tim" "even cooler" conn );
             assert_equal "even cooler" (Redis.string_of_bulk_data (List.hd (Redis.sdiff "rory" ["tim"] conn)) );
             assert_equal 1 (Redis.sdiffstore "bob" "rory" ["tim"] conn);
             
             ignore (Redis.del ["rory"; "tim"] conn);
             ignore (Redis.sadd "rory" "cool" conn);
             assert_equal "cool" (Redis.string_of_bulk_data (Redis.srandmember "rory" conn));
             assert_equal Redis.Nil (Redis.srandmember "non_existent_key" conn);
           ));
        
      "multiple databases" >:: 
        (test 
           (fun conn ->
             Redis.select 1 conn;
             Redis.select 0 conn;

             ignore (Redis.lpush "rory" "1" conn);
             ignore (Redis.lpush "rory" "2" conn);
             ignore (Redis.lpush "rory" "11" conn);
             
             assert_bool "" (Redis.move "rory" 1 conn);
           ));

      "sorted sets" >:: 
        (test 
           (fun conn ->
             assert_bool "" (Redis.zadd "coolest" 42.0 "rory" conn);
             assert_bool "" (Redis.zrem "coolest" "rory" conn);

             ignore (Redis.zadd "coolest" 1.0 "rory" conn);
             ignore (Redis.zadd "coolest" 99.0 "tim" conn);

             let f = Redis.string_of_bulk_data in
             let l = List.map f (Redis.zrange "coolest" 0 1 conn) in
             assert_equal ["rory"; "tim"] l;
             
             let f (a, b) = Redis.string_of_bulk_data a, b in
             let l = List.map f (Redis.zrange_with_scores "coolest" 0 1 conn) in
             assert_equal [("rory", 1.0); ("tim", 99.0)] l;

             let f = Redis.string_of_bulk_data in
             let l = List.map f (Redis.zrevrange "coolest" 0 1 conn) in
             assert_equal ["tim"; "rory"] l;
             
             let f (a, b) = Redis.string_of_bulk_data a, b in
             let l = List.map f (Redis.zrevrange_with_scores "coolest" 0 1 conn) in
             assert_equal [("tim", 99.0); ("rory", 1.0)] l;
             
             assert_equal "rory" (Redis.string_of_bulk_data
                                    (List.hd
                                       (Redis.zrangebyscore
                                          "coolest" 0.0 100.0
                                          ~limit:(Redis.Limit (0, 1)) conn)));
             
             assert_equal 2.0 (Redis.zincrby "coolest" 1.0 "rory" conn);

             assert_equal (Redis.Rank 0) (Redis.zrank "coolest" "rory" conn);
             assert_equal (Redis.Rank 1) (Redis.zrevrank "coolest" "rory" conn);

             assert_equal 2 (Redis.zcard "coolest" conn);
             assert_equal 2.0 (Redis.zscore "coolest" "rory" conn);
             assert_equal 1 (Redis.zremrangebyrank "coolest" 0 0 conn);
             assert_equal 1 (Redis.zremrangebyscore "coolest" 80.0 120.0 conn);
             
             ignore (Redis.del_one "tim" conn);
             ignore (Redis.del_one "rory" conn);
             ignore (Redis.zadd "rory" 10.0 "cool" conn);
             ignore (Redis.zadd "tim" 20.0 "uncool" conn);

             assert_equal 2 (Redis.zunionstore "union" ["rory"; "tim"] conn);
             assert_equal 2 (Redis.zunionstore "union" ["rory"; "tim"] ~aggregate:Redis.Sum conn);
             assert_equal 2 (Redis.zunionstore "union" ["rory"; "tim"] ~aggregate:Redis.Min conn);
             assert_equal 2 (Redis.zunionstore "union" ["rory"; "tim"] ~aggregate:Redis.Max conn);
             
             assert_equal 2 (Redis.zunionstore_with_weights "union" ["rory"; "tim"] [1.0; 0.5] conn);
             assert_equal 2 (Redis.zunionstore_with_weights "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Sum conn);
             assert_equal 2 (Redis.zunionstore_with_weights "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Min conn);
             assert_equal 2 (Redis.zunionstore_with_weights "union" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Max conn);
             
             assert_equal 0 (Redis.zinterstore "inter" ["rory"; "tim"] conn);
             assert_equal 0 (Redis.zinterstore "inter" ["rory"; "tim"] ~aggregate:Redis.Sum conn);
             assert_equal 0 (Redis.zinterstore "inter" ["rory"; "tim"] ~aggregate:Redis.Min conn);
             assert_equal 0 (Redis.zinterstore "inter" ["rory"; "tim"] ~aggregate:Redis.Max conn);
             
             assert_equal 0 (Redis.zinterstore_with_weights "inter" ["rory"; "tim"] [1.0; 0.5] conn);
             assert_equal 0 (Redis.zinterstore_with_weights "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Sum conn);
             assert_equal 0 (Redis.zinterstore_with_weights "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Min conn);
             assert_equal 0 (Redis.zinterstore_with_weights "inter" ["rory"; "tim"] [1.0; 0.5] ~aggregate:Redis.Max conn);
           ));

      "sort" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del_one "rory" conn);
             ignore (Redis.lpush "rory" "1" conn);
             ignore (Redis.lpush "rory" "2" conn);

             assert_equal "2" (Redis.string_of_bulk_data 
                                 (List.hd (Redis.sort "rory" 
                                             ~alpha:Redis.Alpha 
                                             ~order:Redis.Desc conn)));


             assert_equal "2" (Redis.string_of_bulk_data 
                                 (List.hd (Redis.sort "rory" ~pattern:Redis.NoSort conn)));
           ));

      "complex sort" >:: 
        (test 
           (fun conn ->
             (* This requires quite some test data to set up *)
             let fields = ["name"; "yob"] in
             let data = [["Rory"; "1984"]; ["Bob"; "1980"]] in
             let add_record index record =
               let add_field name value =
                 Redis.set (name ^ "_" ^ (string_of_int index)) value conn
               in
               ignore (Redis.rpush "people" (string_of_int index) conn);
               List.iter2 add_field fields record;
               index + 1
             in
             ignore (List.fold_left add_record 1 data);
             ignore (Redis.hset "hash_1" "yob" "1984" conn);
             ignore (Redis.hset "hash_2" "yob" "1980" conn);

             let f = Redis.string_of_bulk_data in
             let l = List.map f (Redis.sort "people"
                                   ~get:(Redis.KeyPattern("name_*")) conn) in
             assert_equal ["Rory"; "Bob"] l;


             let l = List.map f (Redis.sort "people"
                                   ~get:(Redis.FieldPattern("hash_*", "yob")) conn) in
             assert_equal ["1984"; "1980"] l;

             let l = List.map f (List.hd (Redis.sort_get_many "people"
                                            ["name_*"; "yob_*"] 
                                            ~pattern:(Redis.KeyPattern("yob_*"))
                                            conn)) in
             assert_equal ["Bob"; "1980"] l;
             
             let l = List.map f (List.hd (Redis.sort_get_many "people" ["name_*"; "yob_*"] 
                                            ~pattern:(Redis.FieldPattern("hash_*", "yob")) conn)) in
             assert_equal ["Bob"; "1980"] l;

             assert_equal 2 (Redis.sort_and_store "people" ["name_*"] "results" 
                               ~pattern:(Redis.KeyPattern("yob_*")) conn);
           ));

      "hashes" >:: 
        (test 
           (fun conn ->
             ignore (Redis.del ["rory"] conn);
             assert_bool "" (Redis.hset "rory" "cool" "true" conn);
             assert_bool "" (not (Redis.hset "rory" "cool" "false" conn));
             
             assert_bool "" (Redis.hdel "rory" "cool" conn);
             assert_bool "" (not (Redis.hdel "rory" "cool" conn));
             
             assert_bool "" (Redis.hset "rory" "handsome" "true" conn);
             assert_equal (Redis.String "true") (Redis.hget "rory" "handsome" conn);
             assert_equal Redis.Nil (Redis.hget "rory" "boring" conn);
             
             assert_equal [Redis.String("true"); Redis.Nil] 
               (Redis.hmget "rory" ["handsome"; "boring"] conn);

             Redis.hmset "rory" [("handsome", "false"); ("boring", "true")] conn;

             assert_equal 26 (Redis.hincrby "rory" "age" 26 conn);

             assert_bool "" (Redis.hexists "rory" "handsome" conn);
             assert_bool "" (not (Redis.hexists "rory" "andsome" conn));
    
             assert_equal 3 (Redis.hlen "rory" conn);
             
             ignore (Redis.del ["rory"] conn);
             ignore (Redis.hset "rory" "cool" "true" conn);
             assert_equal ["cool"] (Redis.hkeys "rory" conn);

             assert_equal ["true"] (Redis.hvals "rory" conn);

             assert_equal [("cool", "true")] (Redis.hgetall "rory" conn);
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
             ignore (Redis.del ["rory"; "tim"; "bob"] conn);
             ignore (Redis.rpush "rory" "cool" conn);

             assert_equal "cool" (Redis.string_of_bulk_data (Redis.blpop "rory" conn));
             assert_equal Redis.Nil (Redis.blpop "rory" ~timeout:(Redis.Seconds 1) conn);

             ignore (Redis.rpush "tim" "not cool" conn);
             assert_equal ("tim", Redis.String "not cool") (Redis.blpop_many ["rory"; "tim"] conn);
             assert_equal ("", Redis.Nil) (Redis.blpop_many ["rory"; "tim"] 
                                             ~timeout:(Redis.Seconds 1) conn);

             ignore (Redis.rpush "rory" "cool" conn);
             assert_equal "cool" (Redis.string_of_bulk_data (Redis.brpop "rory" conn));
             assert_equal Redis.Nil (Redis.brpop "rory" ~timeout:(Redis.Seconds 1) conn);

             ignore (Redis.rpush "tim" "not cool" conn);
             assert_equal ("tim", Redis.String("not cool")) (Redis.blpop_many ["rory"; "tim"; "bob"] conn);
             assert_equal ("", Redis.Nil) (Redis.blpop_many ["rory"; "tim"; "bob"] 
                                             ~timeout:(Redis.Seconds 1) conn);
           ));

      "persistence" >:: 
        (test 
           (fun conn ->
             Redis.save conn;
             Redis.bgsave conn;

             assert_bool "" (0.0 < Redis.lastsave conn);
             Redis.bgrewriteaof conn;
           ));
    ]

let shutdown_suite = 
  let setup () : Redis.Connection.t = 
    let conn = Redis.create_connection () in
    Redis.auth "qwerty" conn;
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
             Redis.auth "qwerty" conn;
             Redis.shutdown conn;
           ));
    ]
