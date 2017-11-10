module Test = struct
  type person = { name : string ; age : int }

  type company = { companyName : string ; employees : person list }

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
end

(* need to turn on servant server before running *)
let () =
  let person  : Test.person = {name = "Javier" ; age = 50} in
  let person2 : Test.person = {name = "Joaquim" ; age = 45} in
  let person3 : Test.person = {name = "Jordi" ; age = 23} in
  let company : Test.company = {companyName = "Acme" ; employees = [person ; person2 ; person3]  } in
(*
  AesonSpec.server_roundtrip Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" person;

  AesonSpec.server_roundtrip Test.decodeCompany Test.encodeCompany "company" "http://localhost:8081/company" company;
  
  AesonSpec.server_roundtrip_set Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" [person ; person2 ; person3];

  AesonSpec.file_roundtrip Test.decodePerson Test.encodePerson "person" "__tests__/person.json";
  
  AesonSpec.sample_roundtrip Test.decodePerson Test.encodePerson "person" "__tests__/sample.json";

  AesonSpec.golden Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" "__tests__/sample.json";
  *)
  AesonSpec.ggolden Test.decodePerson Test.encodePerson "person" "http://localhost:8081/people" "__tests__/golden/Person.json";
  (* can run tests this way yet since Jest does not expost the assert type constructor
  describe "" (fun () -> test "" (fun () ->
    expect (AesonSpec.file_roundtrip2 "__tests__/person.json" Test.decodePerson Test.brokenEncodePerson) |> toEqual (Ok)
  ))
   *)
