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


let getJsonSamples json =
  match Js.Json.decodeObject json with
  | Some dict ->
     (match Js_dict.get dict "samples" with
      | Some keyValue -> Js.Json.decodeArray keyValue
      | _ -> None
     )
  | _ -> None
  
(* let sampleJsonRoundtripSpec decode encode json =
 *   let rDecoded = decodeSample decode json in
 *   expect (resultMap (fun encoded -> encodeSample encode encoded) rDecoded) |> toEqual (Belt.Result.Ok json) *)

(* let getFirstFail xs =
 *   let ys = List.fold_left
 *     (fun a b ->
 *       match b with
 *       | Expect.Ok -> []
 *       | Expect.Fail _ -> a @ [b]
 *     ) [] xs in
 *   nth_opt ys 0 *)
       
let sampleJsonRoundtripSpec decode encode json =
  let rDecoded = decodeSample decode json in
  match rDecoded with
  | Belt.Result.Ok decoded ->
     (let encoded = encodeSample encode decoded in

      let a = getJsonSamples encoded in
      let b = getJsonSamples json in
      (match ((a,b)) with
       | (Some(c),Some(d)) ->
          let z = Belt.List.zip (Array.to_list c) (Array.to_list d) in
          let xs = List.map (fun ((x,y)) -> expect x |> toEqual y) z in
          let s = getFirstFail xs in
          fail ""
                 
       | _ -> fail ""
      )

     )
  | _ -> fail ""
  
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

let isJsonFile fileName =
  let items = Array.to_list (Js.String.split "." fileName) in
  let length = Js.List.length items in
  match Js.List.nth items (length - 1) with
  | Some ext -> ext == "json"
  | None -> false

(* run roundtrip file test on a directory *)
let goldenDirSpec decode encode name_of_type json_dir =
  let files_in_dir = (Js.Array.filter isJsonFile (Node.Fs.readdirSync json_dir)) in
  Array.iter (fun json_file -> sampleGoldenSpec decode encode name_of_type (json_dir ^ "/" ^ json_file);) files_in_dir

let decodeIntWithResult json =
  Aeson.Decode.wrapResult Aeson.Decode.int json
