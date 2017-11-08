val server_roundtrip : 'a -> string -> (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> unit Js.Promise.t

val file_roundtrip : string -> (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> unit
