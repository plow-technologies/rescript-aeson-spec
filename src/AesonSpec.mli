type 'a sample =
  { seed : float
  ; samples : 'a list
  }
  
val decodeSampleUnsafe : (Js_json.t -> ('a, string) Js_result.t) -> Js_json.t -> 'a sample

(** decode sample from a JSON but throw an exception if it is unable to decode *)
  
val decodeSample : (Js_json.t -> ('a, string) Js_result.t) -> Js_json.t -> ('a sample, string) Js_result.t

(** decode sample from a JSON and return it as Js_result *)
  
val encodeSample : ('a -> Js_json.t) -> 'a sample -> Js_json.t

(** encode sample into a JSON *)
  
(* specs *)  

val jsonRoundtripSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> Js_json.t -> (Js_json.t, string) Js.Result.t Jest.assertion

(** try to encode and decode a JSON *)
  
val sampleJsonRoundtripSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> Js_json.t -> (Js_json.t, string) Js.Result.t Jest.assertion

(** try to encode and decode a sample JSON of a type *)
  
val valueRoundtripSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> 'a -> ('a, string) Js.Result.t Jest.assertion

(** try to encode and decode a value of a type *)
  
val goldenSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> unit

(** decode and encode a golden file *)
  
val sampleGoldenSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> unit

(** decode and encode a golden file for a sample of a type *)
  
val serverSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> 'a -> unit

(** encode a value, POST it to a test server, receive a response and decode the response *)
  
val sampleServerSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> 'a list -> unit

(** encode a sample of a type, POST it to a test server, receive a response and decode the response *)

val isJsonFile : string -> bool

(** test if file name ends in ".json" *)

val sampleGoldenAndServerSpec : (Js_json.t -> ('a, string) Js_result.t) -> ('a -> Js_json.t) -> string -> string -> string -> unit  

(** goldenSpec and sampleServerSpec *)  

val decodeIntWithResult : Js_json.t -> (int, string) Js_result.t

(** helper function for ocaml-export *)
