open Jest
open Expect

(* types *)

type 'a sample =
  { seed : float
  ; samples : 'a list
  }
  
let decodeSampleUnsafe decode json =  
  { seed = Aeson.Decode.field "seed" Aeson.Decode.float json
  ; samples = Aeson.Decode.field "samples" (Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a))) json
  }

let decodeSample decode json =
  match decodeSampleUnsafe decode json with
  | v -> Js_result.Ok v
  | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodeSample: " ^ message)

let encodeSample encode sample =
  Aeson.Encode.object_
    [ ( "seed",  Aeson.Encode.float sample.seed )
    ; ( "samples", Aeson.Encode.list encode sample.samples )
    ]

(* internal functions *)

external toJsObject : 'a Js.Dict.t -> < .. > Js.t = "%identity"

let resultMap f r = (
  match r with
  | Js_result.Ok(a) -> Js_result.Ok (f a)
  | Js_result.Error(b) -> Js_result.Error (b)
)

(* external functions *)

(* roundtrip spec : given an object 'o', encode 'o' then decode the result, the decoded result must equal 'o'. *)

let jsonRoundtripSpec decode encode json =
  let rDecoded = decode json in
  expect (resultMap encode rDecoded) |> toEqual (Js_result.Ok json)
                  
let sampleJsonRoundtripSpec decode encode json =
  let rDecoded = decodeSample decode json in
  expect (resultMap (fun encoded -> encodeSample encode encoded) rDecoded) |> toEqual (Js_result.Ok json)

let valueRoundtripSpec decode encode value =
  expect (decode (encode value)) |> toEqual (Js_result.Ok value)

(* file tests *)

let goldenSpec decode encode name_of_type json_file = (
  describe ("AesonSpec.goldenSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
    let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
    test ("decode then encode: " ^ (Js.Json.stringify json)) (fun () ->
      jsonRoundtripSpec decode encode json
    )
  )
)
                                                    
let sampleGoldenSpec decode encode name_of_type json_file = (
  describe ("AesonSpec.sampleGoldenSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
    let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
    test "decode then encode json_file" (fun () ->
      sampleJsonRoundtripSpec decode encode json
    )
  )
)

(* server tests *)

let serverSpec decode encode name_of_type url value = (
  let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (encode value) in
  let reqInit = 
    Bs_node_fetch.RequestInit.make
      ~method_:Bs_node_fetch.Post
      ~mode:Bs_node_fetch.CORS
      ~body:(Bs_node_fetch.BodyInit.make encodedString)
      ~headers:headers
      () in
  
  describe ("AesonSpec.serverSpec: " ^ name_of_type) (fun () ->
    testPromise ("encode, POST to server, receieve from server, decode: " ^ encodedString) (fun () ->
      Js.Promise.(
        Bs_node_fetch.fetchWithInit url reqInit
          |> then_ (fun response -> (Bs_node_fetch.Response.text response)
          |> then_ (fun text -> resolve (expect (Aeson.Decode.unwrapResult (decode (Js.Json.parseExn text))) |> toEqual value))
        )
      )
    )
  )
)

let sampleServerSpec decode encode name_of_type url values = (
  let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (Aeson.Encode.list encode values) in
  let reqInit = 
    Bs_node_fetch.RequestInit.make
      ~method_:Bs_node_fetch.Post
      ~mode:Bs_node_fetch.CORS
      ~body:(Bs_node_fetch.BodyInit.make encodedString)
      ~headers:headers
      () in
  
  describe ("AesonSpec.sampleServerSpec: " ^ name_of_type) (fun () ->
    testPromise "encode json_file, POST encoded to server, receieve response from server, decode response" (fun () ->
      Js.Promise.(
        Bs_node_fetch.fetchWithInit url reqInit
          |> then_ (fun response -> (Bs_node_fetch.Response.text response)
          |> then_ (fun text -> resolve (expect ((Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a)) (Js.Json.parseExn text))) |> toEqual values))
        )
      )
    )
  )
)

(* golden file and server tests *)
                                                           
let sampleGoldenAndServerSpec decode encode name_of_type url json_file =
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
  match (decodeSample decode json) with 
  | Js_result.Ok sample -> (
    describe ("AesonSpec.sampleGoldenAndServerSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
               
      test "decode then encode json_file" (fun () ->
        expect (encodeSample encode sample) |> toEqual json
      );

      let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
      let encodedString = Js.Json.stringify (Aeson.Encode.list encode sample.samples) in
      let reqInit = 
        Bs_node_fetch.RequestInit.make
          ~method_:Bs_node_fetch.Post
          ~mode:Bs_node_fetch.CORS
          ~body:(Bs_node_fetch.BodyInit.make encodedString)
          ~headers:headers
          () in

      testPromise "encode json_file, POST encoded to server, receieve response from server, decode response" (fun () ->
        Js.Promise.(
          Bs_node_fetch.fetchWithInit url reqInit
            |> then_ (fun response -> (Bs_node_fetch.Response.text response)
            |> then_ (fun text -> resolve (expect ((Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a)) (Js.Json.parseExn text))) |> toEqual sample.samples))
          )
        )
      )
    )
  )
  | Js_result.Error message -> describe "" (fun () -> test "" (fun () -> fail message))
