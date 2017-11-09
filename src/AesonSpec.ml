open Jest
open Expect

external toJsObject : 'a Js.Dict.t -> < .. > Js.t = "%identity"
  
let server_roundtrip decode encode name_of_type url value_of_type  = (
  let headers = Bs_node_fetch.HeadersInit.make (toJsObject (Js_dict.fromList [("Content-Type", Js_json.string "application/json")])) in
  let encodedString = Js.Json.stringify (encode value_of_type) in
  let reqInit = 
    Bs_node_fetch.RequestInit.make
      ~method_:Bs_node_fetch.Post
      ~mode:Bs_node_fetch.CORS
      ~body:(Bs_node_fetch.BodyInit.make encodedString)
      ~headers:headers
      () in
  
  describe ("AesonSpec.server_roundtrip: " ^ name_of_type) (fun () ->
    testPromise ("send to and receive from server: " ^ encodedString) (fun () ->
      Js.Promise.(
        Bs_node_fetch.fetchWithInit url reqInit
          |> then_ (fun response -> (Bs_node_fetch.Response.text response)
          |> then_ (fun text -> Js.log text; resolve (expect (decode (Js.Json.parseExn text)) |> toEqual (Js_result.Ok value_of_type)))
        )
      )
    )
  )
)

let server_roundtrip_set decode encode name_of_type url values_of_type =
  List.iter (fun (value_of_type) -> server_roundtrip decode encode name_of_type url value_of_type) values_of_type

let file_roundtrip decode encode name_of_type json_file  = (
  let mapJsResult f r = (
    match r with
    | Js_result.Ok(a) -> Js_result.Ok (f a)
    | Js_result.Error(b) -> Js_result.Error (b)
  ) in

  describe ("AesonSpec.file_roundtrip: " ^ name_of_type ^ " from file '" ^ json_file ^ "'") (fun () ->
    let json = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync json_file) in
    test ("decode then encode: " ^ (Js.Json.stringify json)) (fun () ->
      let rDecoded = decode json in
      expect (mapJsResult encode rDecoded) |> toEqual (Js_result.Ok json)
    )
  );
)

(* use this if we can match on Jest.assert constructors
let file_roundtrip2 file decode encode = (
  let mapJsResult f r = (
    match r with
    | Js_result.Ok (a) -> Js_result.Ok(f a)
    | Js_result.Error(b) -> Js_result.Error(b)
  )  in

  let f = Js.Json.parseExn (Node.Fs.readFileAsUtf8Sync file) in
  let rDecoded = decode f in
  expect (mapJsResult encode rDecoded) |> toEqual (Js_result.Ok f)
)
 *)
