val server_roundtrip : string -> 'a -> string -> (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> unit

val file_roundtrip : string -> string -> (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> unit

(*
val file_roundtrip2 : string -> (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> ((Js.Json.t, string) Js.Result.t) Jest.assertion
 *)
