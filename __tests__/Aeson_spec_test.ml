module Test = struct
  type person = { name : string ; age : int }

  let encodePerson (p : person) :Js_json.t =
    Aeson.Encode.object_
      [ ( "name", Aeson.Encode.string p.name)
      ; ( "age", Aeson.Encode.int p.age)
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

let () =
  Js.log "Hello";
         (* let javier : person = { name : "Javier" ; age : 45 } *)
  Aeson2.Spec.file_roundtrip "person" "__tests__/person.json" Test.decodePerson Test.encodePerson;
