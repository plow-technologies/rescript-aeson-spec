type 'a sample =
  { seed : float
  ; samples : 'a list
  }
  
val decodeSampleUnsafe : (Js_json.t -> ('a, string) Belt.Result.t) -> Js_json.t -> 'a sample

(** decode sample from a JSON but throw an exception if it is unable to decode *)
  
val decodeSample : (Js_json.t -> ('a, string) Belt.Result.t) -> Js_json.t -> ('a sample, string) Belt.Result.t

(** decode sample from a JSON and return it as Belt.Result *)
  
val encodeSample : ('a -> Js_json.t) -> 'a sample -> Js_json.t

(** encode sample into a JSON *)
  
(* specs *)  

val jsonRoundtripSpec : (Js_json.t -> ('a, string) Belt.Result.t) -> ('a -> Js_json.t) -> Js_json.t -> InternalJest.assertion
  
(** try to encode and decode a JSON *)
  
val sampleJsonRoundtripSpec : (Js_json.t -> ('a, string) Belt.Result.t) -> ('a -> Js_json.t) -> Js_json.t -> InternalJest.assertion

(** try to encode and decode a sample JSON of a type *)

val valueRoundtripSpec : (Js_json.t -> ('a, string) Belt.Result.t) -> ('a -> Js_json.t) -> 'a -> InternalJest.assertion

(** try to encode and decode a value of a type *)
  
val goldenSpec : (Js_json.t -> ('a, string) Belt.Result.t) -> ('a -> Js_json.t) -> string -> string -> unit

(** decode and encode a golden file *)
  
val sampleGoldenSpec : (Js_json.t -> ('a, string) Belt.Result.t) -> ('a -> Js_json.t) -> string -> string -> unit

(** encode a sample of a type, POST it to a test server, receive a response and decode the response *)

val isJsonFile : string -> bool
  
(** test if file name ends in ".json" *)

val goldenDirSpec : (Js_json.t -> ('a, string) Belt.Result.t) -> ('a -> Js_json.t) -> string -> string -> unit

(** goldenSpec and sampleServerSpec *)  

val decodeIntWithResult : Js_json.t -> (int, string) Belt.Result.t
