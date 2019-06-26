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
  | v -> Belt.Result.Ok v
  | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodeSample: " ^ message)

let encodeSample encode sample =
  Aeson.Encode.object_
    [ ( "seed",  Aeson.Encode.float sample.seed )
    ; ( "samples", Aeson.Encode.list encode sample.samples )
    ]

(* internal functions *)

external toJsObject : 'a Js.Dict.t -> < .. > Js.t = "%identity"

let resultMap f r = (
  match r with
  | Belt.Result.Ok(a) -> Belt.Result.Ok (f a)
  | Belt.Result.Error(b) -> Belt.Result.Error (b)
)

(* external functions *)

(* roundtrip spec : given an object 'o', encode 'o' then decode the result, the decoded result must equal 'o'. *)

let jsonRoundtripSpec decode encode json =
  let rDecoded = decode json in
  expect (resultMap encode rDecoded) |> toEqual (Belt.Result.Ok json)
                  
let sampleJsonRoundtripSpec decode encode json =
  let rDecoded = decodeSample decode json in
  expect (resultMap (fun encoded -> encodeSample encode encoded) rDecoded) |> toEqual (Belt.Result.Ok json)

let valueRoundtripSpec decode encode value =
  expect (decode (encode value)) |> toEqual (Belt.Result.Ok value)

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
  let headers = Fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (encode value) in
  let reqInit = 
    Fetch.RequestInit.make
      ~method_:Fetch.Post
      ~mode:Fetch.CORS
      ~body:(Fetch.BodyInit.make encodedString)
      ~headers:headers
      () in
  
  describe ("AesonSpec.serverSpec: " ^ name_of_type) (fun () ->
    testPromise ("encode, POST to server, receieve from server, decode: " ^ encodedString) (fun () ->
      Js.Promise.(
        Fetch.fetchWithInit url reqInit
          |> then_ (fun response -> (Fetch.Response.text response)
          |> then_ (fun text -> resolve (expect (Aeson.Decode.unwrapResult (decode (Js.Json.parseExn text))) |> toEqual value))
        )
      )
    )
  )
)

let sampleServerSpec decode encode name_of_type url values = (
  let headers = Fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (Aeson.Encode.list encode values) in
  let reqInit = 
    Fetch.RequestInit.make
      ~method_:Fetch.Post
      ~mode:Fetch.CORS
      ~body:(Fetch.BodyInit.make encodedString)
      ~headers:headers
      () in
  
  describe ("AesonSpec.sampleServerSpec: " ^ name_of_type) (fun () ->
    testPromise "encode json_file, POST encoded to server, receieve response from server, decode response" (fun () ->
      Js.Promise.(
        Fetch.fetchWithInit url reqInit
          |> then_ (fun response -> (Fetch.Response.text response)
          |> then_ (fun text -> resolve (expect ((Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a)) (Js.Json.parseExn text))) |> toEqual values))
        )
      )
    )
  )
)

let isJsonFile fileName =
  let items = Array.to_list (Js.String.split "." fileName) in
  let length = Js.List.length items in
  match Js.List.nth items (length - 1) with
  | Some ext -> ext == "json"
  | None -> false
                                                           
(* golden file and server tests *)
                                                           
let sampleGoldenAndServerFileSpec decode encode name_of_type url json_file =
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
  match (decodeSample decode json) with 
  | Belt.Result.Ok sample -> (
    describe ("AesonSpec.sampleGoldenAndServerSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
               
      test "decode then encode json_file" (fun () ->
        expect (encodeSample encode sample) |> toEqual json
      );

      let headers = Fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
      let encodedString = Js.Json.stringify (Aeson.Encode.list encode sample.samples) in
      let reqInit = 
        Fetch.RequestInit.make
          ~method_:Fetch.Post
          ~mode:Fetch.CORS
          ~body:(Fetch.BodyInit.make encodedString)
          ~headers:headers
          () in

      testPromise "encode json_file, POST encoded to server, receieve response from server, decode response" (fun () ->
        Js.Promise.(
          Fetch.fetchWithInit url reqInit
            |> then_ (fun response -> (Fetch.Response.text response)
            |> then_ (fun text -> resolve (expect ((Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a)) (Js.Json.parseExn text))) |> toEqual sample.samples))
          )
        )
      )
    )
  )
  | Belt.Result.Error message -> describe "" (fun () -> test "" (fun () -> fail message))

(* run roundtrip file test on a directory *)
let goldenDirSpec decode encode name_of_type json_dir =
  let files_in_dir = (Js.Array.filter isJsonFile (Node.Fs.readdirSync json_dir)) in
  Array.iter (fun json_file -> sampleGoldenSpec decode encode name_of_type (json_dir ^ "/" ^ json_file);) files_in_dir

let sampleGoldenAndServerSpec decode encode name_of_type url json_dir =
  let filesInDir = (Js.Array.filter isJsonFile (Node.Fs.readdirSync json_dir)) in
  Js.log filesInDir;
  Array.iter (fun json_file -> sampleGoldenAndServerFileSpec decode encode name_of_type url (json_dir ^ "/" ^ json_file);) filesInDir

let decodeIntWithResult json =
  Aeson.Decode.wrapResult Aeson.Decode.int json
