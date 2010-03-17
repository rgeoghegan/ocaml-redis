module Connection :
    sig
        type t = in_channel * out_channel
        val create : string -> int -> t
        val read_string : t -> string
        val read_fixed_string : int -> t -> string
        val send_text_straight : string -> t -> unit
        val send_text : string -> t -> unit
        val get_one_char : t -> char
        val flush_connection : t -> unit
    end

module Redis_util :
    sig
        type redis_value_type = RedisString | RedisNil | RedisList | RedisSet
        type bulk_data = Nil | String of string
        val string_of_bulk_data : bulk_data -> string
        type response =
            Status of string
          | Undecipherable
          | Integer of int
          | LargeInteger of float
          | Bulk of bulk_data
          | Multibulk of bulk_data list
          | Error of string
        val string_of_response : response -> string
        val receive_answer : Connection.t -> response
        val send_and_receive_command : string -> Connection.t -> response
        val send_and_receive_command_safely : string -> Connection.t -> response
        val aggregate_command : string -> string list -> string
        val send_multibulk_command :
          string list -> Connection.t -> response
        val handle_special_status : string -> response -> unit
        val handle_status : response -> unit
        val handle_integer : response -> bool
        val handle_float : response -> float
    end
