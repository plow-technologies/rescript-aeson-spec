type sample =
  { seed : int
  ; samples : Js_json.t array
  }

val decode_sample_unsafe : Js_json.t -> sample
  
val decode_sample : Js_json.t -> (sample, string) Js_result.t

val server_roundtrip : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> 'a -> unit

(** Test a decoder and encoder of a type for a given value against a test server *)

val server_roundtrip_set : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> 'a list -> unit

(** Test a decoder and encoder of a type for a list of values against a test server *)
  
val file_roundtrip : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> unit

(** test a decoder and encoder of a type against a json value in a file *)


val golden : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> unit
(*
val file_roundtrip2 : string -> (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> ((Js.Json.t, string) Js.Result.t) Jest.assertion
 *)
