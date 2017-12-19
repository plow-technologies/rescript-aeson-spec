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

  let decodePerson (json : Js_json.t) :(person, string) Js_result.t =
    match Aeson.Decode.
          { name = field "name" string json
          ; age = field "age" int json
          }
    with
    | v -> Js_result.Ok v
    | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)

  let encodeCompany (p : company) :Js_json.t =
    Aeson.Encode.object_
      [ ( "companyName", Aeson.Encode.string p.companyName)
      ; ( "employees", Aeson.Encode.list encodePerson p.employees)
      ]

  let decodeCompany (json : Js_json.t) :(company, string) Js_result.t =
    match Aeson.Decode.
          { companyName = field "companyName" string json
          ; employees = field "employees" (list (fun a -> unwrapResult (decodePerson a))) json
          }
    with
    | v -> Js_result.Ok v
    | exception Aeson.Decode.DecodeError message -> Js_result.Error ("decodePerson: " ^ message)

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

  let decodeShape (json : Js_json.t) :(shape, string) Js_result.t =
    match Aeson.Decode.(field "tag" string json) with
    | "Square" ->
       (match Aeson.Decode.(field "contents" Js.Json.decodeArray json) with
        | Some v ->
           (match Aeson.Decode.int v.(0) with
            | v0 ->
               (match Aeson.Decode.int v.(1) with
                | v1 ->
                   Js_result.Ok (Square (v0, v1))
                | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Square: " ^ message)
               )
            | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Square: " ^ message)
           )
        | None -> Js_result.Error ("Square expected an array.")
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
                       Js_result.Ok (Triangle (v0, v1, v2))
                    | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Triangle: " ^ message)
                   )
                | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Triangle: " ^ message)
               )
            | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Triangle: " ^ message)
           )
        | None -> Js_result.Error ("Triangle expected an array.")
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
                           Js_result.Ok (Rectangle (v0, v1, v2, v3))
                        | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Rectangle: " ^ message)
                       )
                    | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Rectangle: " ^ message)
                   )
                | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Rectangle: " ^ message)
               )
            | exception Aeson.Decode.DecodeError message -> Js_result.Error ("Rectangle: " ^ message)
           )
        | None -> Js_result.Error ("Rectangle expected an array.")
       )
    | err -> Js_result.Error ("Unknown tag value found '" ^ err ^ "'.")
    | exception Aeson.Decode.DecodeError message -> Js_result.Error message

end

(* need to turn on servant server before running *)
let () =
  let person  : Test.person = {name = "Javier" ; age = 50} in
  let person2 : Test.person = {name = "Joaquim" ; age = 45} in
  let person3 : Test.person = {name = "Jordi" ; age = 23} in
  let company : Test.company = {companyName = "Acme" ; employees = [person ; person2 ; person3]  } in

  AesonSpec.goldenSpec Test.decodePerson Test.encodePerson "person" "__tests__/golden/Person.json";

  AesonSpec.serverSpec Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" person;

  AesonSpec.serverSpec Test.decodeCompany Test.encodeCompany "company" "http://localhost:8081/company" company;

  AesonSpec.sampleGoldenAndServerSpec Test.decodePerson Test.encodePerson "person" "http://localhost:8081/people" "__tests__/golden/Person";

  AesonSpec.sampleGoldenAndServerSpec Test.decodeCompany Test.encodeCompany "company" "http://localhost:8081/companies" "__tests__/golden/Company";

  AesonSpec.sampleGoldenAndServerSpec Test.decodeShape Test.encodeShape "shape" "http://localhost:8081/shapes" "__tests__/golden/Shape";
  
  describe "isJsonFile" (fun () ->         
    test "" (fun () ->
      let files = Node.Fs.readdirSync "__tests__/golden/Person" in

      let filename = Array.get files 0 in

      expect (AesonSpec.isJsonFile filename) |> toEqual true;
    )
  )
