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

  AesonSpec.goldenSingleSpec Test.decodePerson Test.encodePerson "person" "__tests__/person.json";

  AesonSpec.serverSingleSpec Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" person;

  AesonSpec.serverSingleSpec Test.decodeCompany Test.encodeCompany "company" "http://localhost:8081/company" company;

  AesonSpec.goldenAndServerSpec Test.decodePerson Test.encodePerson "person" "http://localhost:8081/people" "__tests__/golden/Person.json";
  (*
  AesonSpec.serverSpec Test.decodePerson Test.encodePerson "person" "http://localhost:8081/person" [person ; person2 ; person3];
  

  *)

(*


  *)
