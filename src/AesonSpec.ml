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

let jsonRoundtripSingleSpec decode encode json =
  let rDecoded = decode json in
  expect (resultMap encode rDecoded) |> toEqual (Js_result.Ok json)
                  
let jsonRoundtripSpec decode encode json =
  let rDecoded = decodeSample decode json in
  expect (resultMap (fun encoded -> encodeSample encode encoded) rDecoded) |> toEqual (Js_result.Ok json)

let valueRoundtripSpec decode encode value =
  expect (decode (encode value)) |> toEqual (Js_result.Ok value)

(* file tests *)

let goldenSingleSpec decode encode name_of_type json_file = (
  describe ("AesonSpec.goldenSingleSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
    let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
    test ("decode then encode: " ^ (Js.Json.stringify json)) (fun () ->
      jsonRoundtripSingleSpec decode encode json
    )
  )
)
                                                    
let goldenSpec decode encode name_of_type json_file = (
  describe ("AesonSpec.goldenSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
    let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
    test ("decode then encode: " ^ (Js.Json.stringify json)) (fun () ->
      jsonRoundtripSpec decode encode json
    )
  )
)

(* server tests *)

let serverSingleSpec decode encode name_of_type url value = (
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

let serverSpec decode encode name_of_type url values = (
  let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (Aeson.Encode.list encode values) in
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
          |> then_ (fun text -> resolve (expect ((Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a)) (Js.Json.parseExn text))) |> toEqual values))
        )
      )
    )
  )
)


let goldenAndServerSpec decode encode name_of_type url json_file =
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
  match (decodeSample decode json) with 
  | Js_result.Ok sample -> (
    describe ("AesonSpec.goldenAndServerSpec: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
               
      test ("decode then encode: " ^ (Js.Json.stringify json)) (fun () ->
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

      testPromise ("encode, POST to server, receieve from server, decode: " ^ encodedString) (fun () ->
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

(*

 let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
       let encodedString = Js.Json.stringify (encodeSample encode sample) in
       let reqInit = 
         Bs_node_fetch.RequestInit.make
           ~method_:Bs_node_fetch.Post
           ~mode:Bs_node_fetch.CORS
           ~body:(Bs_node_fetch.BodyInit.make encodedString)
           ~headers:headers
           () in

       testPromise ("encode, POST to server, receieve from server, decode: " ^ encodedString) (fun () ->
         Js.Promise.(
           Bs_node_fetch.fetchWithInit url reqInit
             |> then_ (fun response -> (Bs_node_fetch.Response.text response)
             |> then_ (fun text -> resolve (expect (decodeSample decode (Js.Json.parseExn text)) |> toEqual (Js_result.Ok sample)))
           )
         )
       )
     )


let goldenAndServerSpec decode encode url name_of_type json_file = 
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
 *)  
(*
let file_roundtrip decode encode name_of_type json_file  = (
  describe ("AesonSpec.file_roundtrip: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
    let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
    test ("decode then encode: " ^ (Js.Json.stringify json)) (fun () ->
      roundtrip decode encode json
    )
  );
)
 *)
                                                     
(* server tests *)

(* file and server tests *)

(* golden file tests 
   golden server test
   golden roundtrip test
*)
(*  
let server_test decode encode url value_of_type  = (
  let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (encode value_of_type) in
  let reqInit = 
    Bs_node_fetch.RequestInit.make
      ~method_:Bs_node_fetch.Post
      ~mode:Bs_node_fetch.CORS
      ~body:(Bs_node_fetch.BodyInit.make encodedString)
      ~headers:headers
      () in
  

  Js.Promise.(
    Bs_node_fetch.fetchWithInit url reqInit
      |> then_ (fun response -> (Bs_node_fetch.Response.text response)
      |> then_ (fun text -> resolve (expect (decode (Js.Json.parseExn text)) |> toEqual (Js_result.Ok value_of_type)))
    )
  )
)
  
let server_roundtrip decode encode name_of_type url value_of_type = (
  let encodedString = Js.Json.stringify (encode value_of_type) in
  describe ("AesonSpec.server_roundtrip: " ^ name_of_type) (fun () ->
    testPromise ("send to and receive from server: " ^ encodedString) (fun () ->
      server_test decode encode url value_of_type
    )
  )
)

let server_roundtrip_set decode encode name_of_type url values_of_type =
  List.iter (fun (value_of_type) -> server_roundtrip decode encode name_of_type url value_of_type) values_of_type



let sample_roundtrip decode encode name_of_type json_file = (
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
  
  match (decode_sample json) with
  | Js_result.Ok sample -> describe name_of_type (fun () -> 
      List.iter (fun sample -> test "samples" (fun () -> roundtrip decode encode sample)) (Array.to_list sample.samples))
  | Js_result.Error error -> describe "" (fun () -> test "" (fun () -> fail error))
  )


let golden decode encode name_of_type url json_file = (
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
  
  match (decode_sample json) with
  | Js_result.Ok sample ->
     describe ("golden test for: " ^ name_of_type) (fun () -> 
      List.iter (fun sample -> test "file" (fun () -> roundtrip decode encode sample);
                               testPromise "server" (fun () -> server_test decode encode url (Aeson.Decode.unwrapResult (decode sample)));)
                (Array.to_list sample.samples))
  | Js_result.Error error -> describe "" (fun () -> test "" (fun () -> fail error))
  )


let ggolden decode encode name_of_type url json_file = (
  let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
  
  match (decode_ssample decode json) with
  | Js_result.Ok sample ->
     describe ("golden test for: " ^ name_of_type) (fun () ->
       let decoded = sample.samples in
       let encoded = (encode_ssample encode sample) in
       test "golden file" (fun () ->
         expect (encoded) |> toEqual (json);
       );

       testPromise "server" (fun () ->
         let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
         let encodedString = Js.Json.stringify (Aeson.Encode.list encode sample.samples) in
         let reqInit = 
           Bs_node_fetch.RequestInit.make
             ~method_:Bs_node_fetch.Post
             ~mode:Bs_node_fetch.CORS
             ~body:(Bs_node_fetch.BodyInit.make encodedString)
             ~headers:headers
             () in
  

         Js.Promise.(
           Bs_node_fetch.fetchWithInit url reqInit
           |> then_ (fun response -> (Bs_node_fetch.Response.text response)
           |> then_ (fun text -> resolve (expect ((Aeson.Decode.list (fun a -> Aeson.Decode.unwrapResult (decode a)) (Js.Json.parseExn text))) |> toEqual sample.samples))
           )
         )
       );
 
              (* test "file" (fun () -> roundtrip (Aeson.Decode.list decode) (Aeson.Encode.list encode) payload); *)
              (*        testPromise "server" (fun () -> server_test decode encode url (Aeson.Decode.unwrapResult (decode sample)));)*)
     )
  | Js_result.Error error -> describe "" (fun () -> test "" (fun () -> fail error))
  )

 *)
