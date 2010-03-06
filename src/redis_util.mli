val send_text_straight : string -> 'a * out_channel -> unit
val send_text : string -> 'a * out_channel -> unit
type redis_value_type = RedisString | RedisNil | RedisList | RedisSet
type bulk_data = Nil | String of string
val string_of_bulk_data : bulk_data -> string
type response =
    Status of string
  | Undecipherable
  | Integer of int
  | BigInteger of Big_int.big_int
  | Bulk of bulk_data
  | Multibulk of bulk_data list
  | Error of string
val string_of_response : response -> string
val receive_answer : in_channel * 'a -> response
val send_and_receive_command : string -> in_channel * out_channel -> response
val aggregate_command : string -> string list -> string
val send_multibulk_command :
  string list -> in_channel * out_channel -> response
val handle_special_status : string -> response -> unit
val handle_status : response -> unit
val handle_integer : response -> bool
