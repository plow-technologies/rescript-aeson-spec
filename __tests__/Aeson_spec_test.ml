open Jest
open Expect

module Test = struct
  type person = { name : string ; age : int }

  type company = { companyName : string ; employees : person list }

  type shape =
    | Square of int * int
    | Triangle of int * int * int
    | Rectangle of int * int * int * int

  let encodePerson (p : person) :Js_json.t =
    Aeson.Encode.object_
      [ ( "name", Aeson.Encode.string p.name)
      ; ( "age", Aeson.Encode.int p.age)
      ]

  let brokenEncodePerson (p : person) :Js_json.t =
    Aeson.Encode.object_
      [ ( "Name", Aeson.Encode.string p.name)
      ; ( "Age", Aeson.Encode.int p.age)
      ]

  let decodePerson (json : Js_json.t) :(person, string) Belt.Result.t =
    match Aeson.Decode.
          { name = field "name" string json
          ; age = field "age" int json
          }
    with
    | v -> Belt.Result.Ok v
    | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodePerson: " ^ message)

  let encodeCompany (p : company) :Js_json.t =
    Aeson.Encode.object_
      [ ( "companyName", Aeson.Encode.string p.companyName)
      ; ( "employees", Aeson.Encode.list encodePerson p.employees)
      ]

  let decodeCompany (json : Js_json.t) :(company, string) Belt.Result.t =
    match Aeson.Decode.
          { companyName = field "companyName" string json
          ; employees = field "employees" (list (fun a -> unwrapResult (decodePerson a))) json
          }
    with
    | v -> Belt.Result.Ok v
    | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("decodePerson: " ^ message)

  let encodeShape (x : shape) :Js_json.t =
    match x with
    | Square (y0,y1) ->
       Aeson.Encode.object_
         [ ( "tag", Aeson.Encode.string "Square" )
         ; ( "contents" , Aeson.Encode.array [| Aeson.Encode.int y0 ; Aeson.Encode.int y1 |] )
         ]
    | Triangle (y0,y1,y2) ->
       Aeson.Encode.object_
         [ ( "tag", Aeson.Encode.string "Triangle" )
         ; ( "contents" , Aeson.Encode.array [| Aeson.Encode.int y0 ; Aeson.Encode.int y1 ; Aeson.Encode.int y2|])
         ]
    | Rectangle (y0,y1,y2,y3) ->
       Aeson.Encode.object_
         [ ( "tag", Aeson.Encode.string "Rectangle" )
         ; ( "contents" , Aeson.Encode.array [| Aeson.Encode.int y0 ; Aeson.Encode.int y1 ; Aeson.Encode.int y2 ; Aeson.Encode.int y3|])
         ]

  let decodeShape (json : Js_json.t) :(shape, string) Belt.Result.t =
    match Aeson.Decode.(field "tag" string json) with
    | "Square" ->
       (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
        | Some v ->
           (match Aeson.Decode.int v.(0) with
            | v0 ->
               (match Aeson.Decode.int v.(1) with
                | v1 ->
                   Belt.Result.Ok (Square (v0, v1))
                | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Square: " ^ message)
               )
            | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Square: " ^ message)
           )
        | None -> Belt.Result.Error ("Square expected an array.")
       )
    | "Triangle" ->
       (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
        | Some v ->
           (match Aeson.Decode.int v.(0) with
            | v0 ->
               (match Aeson.Decode.int v.(1) with
                | v1 ->
                   (match Aeson.Decode.int v.(2) with
                    | v2 ->
                       Belt.Result.Ok (Triangle (v0, v1, v2))
                    | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Triangle: " ^ message)
                   )
                | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Triangle: " ^ message)
               )
            | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Triangle: " ^ message)
           )
        | None -> Belt.Result.Error ("Triangle expected an array.")
       )
    | "Rectangle" ->
       (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
        | Some v ->
           (match Aeson.Decode.int v.(0) with
            | v0 ->
               (match Aeson.Decode.int v.(1) with
                | v1 ->
                   (match Aeson.Decode.int v.(2) with
                    | v2 ->
                       (match Aeson.Decode.int v.(3) with
                        | v3 ->
                           Belt.Result.Ok (Rectangle (v0, v1, v2, v3))
                        | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Rectangle: " ^ message)
                       )
                    | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Rectangle: " ^ message)
                   )
                | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Rectangle: " ^ message)
               )
            | exception Aeson.Decode.DecodeError message -> Belt.Result.Error ("Rectangle: " ^ message)
           )
        | None -> Belt.Result.Error ("Rectangle expected an array.")
       )
    | err -> Belt.Result.Error ("Unknown tag value found '" ^ err ^ "'.")
    | exception Aeson.Decode.DecodeError message -> Belt.Result.Error message

end

let () =
  AesonSpec.goldenSpec Test.decodePerson Test.encodePerson "person" "__tests__/golden/Person.json";
  
  AesonSpec.goldenDirSpec Test.decodePerson Test.encodePerson "person" "__tests__/golden/Person";

  AesonSpec.goldenDirSpec Test.decodeCompany Test.encodeCompany "company" "__tests__/golden/Company";

  AesonSpec.goldenDirSpec Test.decodeShape Test.encodeShape "shape" "__tests__/golden/Shape";

  AesonSpec.goldenDirSpec Test.decodeShape Test.encodeShape "shape" "__tests__/golden/Broken";
  
  describe "isJsonFile" (fun () ->         
    test "" (fun () ->
      let files = Node.Fs.readdirSync "__tests__/golden/Person" in

      let filename = Array.get files 0 in

      expect (AesonSpec.isJsonFile filename) |> toEqual true;
    )
  )
