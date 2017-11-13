type 'a sample =
  { seed : float
  ; samples : 'a list
  }
  
val decodeSampleUnsafe : (Js_json.t -> ('a, string) Js_result.t) -> Js_json.t -> 'a sample

val decodeSample : (Js_json.t -> ('a, string) Js_result.t) -> Js_json.t -> ('a sample, string) Js_result.t

val encodeSample : ('a -> Js_json.t) -> 'a sample -> Js_json.t

  
(* specs *)  

val jsonRoundtripSingleSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> Js_json.t -> (Js_json.t, string) Js.Result.t Jest.assertion
  
val jsonRoundtripSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> Js_json.t -> (Js_json.t, string) Js.Result.t Jest.assertion
  
val valueRoundtripSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> 'a -> ('a, string) Js.Result.t Jest.assertion

val goldenSingleSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> unit
  
val goldenSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> unit

val serverSingleSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> 'a -> unit
  
val serverSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> 'a list -> unit

val goldenAndServerSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> string -> unit  
