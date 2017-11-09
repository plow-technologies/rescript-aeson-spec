module Test = struct
  type person = { name : string ; age : int }

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
end

(* need to turn on servant server before running *)
let () =
  let person : Test.person = {name = "Javier" ; age = 50} in
  AesonSpec.server_roundtrip Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" person ;
  AesonSpec.file_roundtrip Test.decodePerson Test.encodePerson "person" "__tests__/person.json";

  (* can run tests this way yet since Jest does not expost the assert type constructor
  describe "" (fun () -> test "" (fun () ->
    expect (AesonSpec.file_roundtrip2 "__tests__/person.json" Test.decodePerson Test.brokenEncodePerson) |> toEqual (Ok)
  ))
   *)
