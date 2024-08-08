open AesonSpec_Jest
open Expect

module Test = {
  type person = {name: string, age: int}

  type company = {companyName: string, employees: list<person>}

  type shape =
    | Square(int, int)
    | Triangle(int, int, int)
    | Rectangle(int, int, int, int)

  let encodePerson = (p: person): Js_json.t =>
    Aeson.Encode.object_(list{
      ("name", Aeson.Encode.string(p.name)),
      ("age", Aeson.Encode.int(p.age)),
    })

  let brokenEncodePerson = (p: person): Js_json.t =>
    Aeson.Encode.object_(list{
      ("Name", Aeson.Encode.string(p.name)),
      ("Age", Aeson.Encode.int(p.age)),
    })

  let decodePerson = (json: Js_json.t): Belt.Result.t<person, string> =>
    switch {
      open Aeson.Decode
      {
        name: field("name", string, json),
        age: field("age", int, json),
      }
    } {
    | v => Belt.Result.Ok(v)
    | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("decodePerson: " ++ message)
    }

  let brokenDecodePerson = (json: Js_json.t): Belt.Result.t<person, string> =>
    switch {
      open Aeson.Decode
      {
        name: field("name!!!!", string, json),
        age: field("age!!!!!", int, json),
      }
    } {
    | v => Belt.Result.Ok(v)
    | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("decodePerson: " ++ message)
    }

  let encodeCompany = (p: company): Js_json.t =>
    Aeson.Encode.object_(list{
      ("companyName", Aeson.Encode.string(p.companyName)),
      ("employees", Aeson.Encode.list(encodePerson, p.employees)),
    })

  let decodeCompany = (json: Js_json.t): Belt.Result.t<company, string> =>
    switch {
      open Aeson.Decode
      {
        companyName: field("companyName", string, json),
        employees: field("employees", list(a => unwrapResult(decodePerson(a))), json),
      }
    } {
    | v => Belt.Result.Ok(v)
    | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("decodePerson: " ++ message)
    }

  let encodeShape = (x: shape): Js_json.t =>
    switch x {
    | Square(y0, y1) =>
      Aeson.Encode.object_(list{
        ("tag", Aeson.Encode.string("Square")),
        ("contents", Aeson.Encode.array([Aeson.Encode.int(y0), Aeson.Encode.int(y1)])),
      })
    | Triangle(y0, y1, y2) =>
      Aeson.Encode.object_(list{
        ("tag", Aeson.Encode.string("Triangle")),
        (
          "contents",
          Aeson.Encode.array([Aeson.Encode.int(y0), Aeson.Encode.int(y1), Aeson.Encode.int(y2)]),
        ),
      })
    | Rectangle(y0, y1, y2, y3) =>
      Aeson.Encode.object_(list{
        ("tag", Aeson.Encode.string("Rectangle")),
        (
          "contents",
          Aeson.Encode.array([
            Aeson.Encode.int(y0),
            Aeson.Encode.int(y1),
            Aeson.Encode.int(y2),
            Aeson.Encode.int(y3),
          ]),
        ),
      })
    }

  let decodeShape = (json: Js_json.t): Belt.Result.t<shape, string> =>
    switch {
      open Aeson.Decode
      field("tag", string, json)
    } {
    | "Square" =>
      switch {
        open Aeson.Decode
        field("contents", Js.Json.decodeArray, json)
      } {
      | Some(v) =>
        switch Aeson.Decode.int(v[0]) {
        | v0 =>
          switch Aeson.Decode.int(v[1]) {
          | v1 => Belt.Result.Ok(Square(v0, v1))
          | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("Square: " ++ message)
          }
        | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("Square: " ++ message)
        }
      | None => Belt.Result.Error("Square expected an array.")
      }
    | "Triangle" =>
      switch {
        open Aeson.Decode
        field("contents", Js.Json.decodeArray, json)
      } {
      | Some(v) =>
        switch Aeson.Decode.int(v[0]) {
        | v0 =>
          switch Aeson.Decode.int(v[1]) {
          | v1 =>
            switch Aeson.Decode.int(v[2]) {
            | v2 => Belt.Result.Ok(Triangle(v0, v1, v2))
            | exception Aeson.Decode.DecodeError(message) =>
              Belt.Result.Error("Triangle: " ++ message)
            }
          | exception Aeson.Decode.DecodeError(message) =>
            Belt.Result.Error("Triangle: " ++ message)
          }
        | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("Triangle: " ++ message)
        }
      | None => Belt.Result.Error("Triangle expected an array.")
      }
    | "Rectangle" =>
      switch {
        open Aeson.Decode
        field("contents", Js.Json.decodeArray, json)
      } {
      | Some(v) =>
        switch Aeson.Decode.int(v[0]) {
        | v0 =>
          switch Aeson.Decode.int(v[1]) {
          | v1 =>
            switch Aeson.Decode.int(v[2]) {
            | v2 =>
              switch Aeson.Decode.int(v[3]) {
              | v3 => Belt.Result.Ok(Rectangle(v0, v1, v2, v3))
              | exception Aeson.Decode.DecodeError(message) =>
                Belt.Result.Error("Rectangle: " ++ message)
              }
            | exception Aeson.Decode.DecodeError(message) =>
              Belt.Result.Error("Rectangle: " ++ message)
            }
          | exception Aeson.Decode.DecodeError(message) =>
            Belt.Result.Error("Rectangle: " ++ message)
          }
        | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("Rectangle: " ++ message)
        }
      | None => Belt.Result.Error("Rectangle expected an array.")
      }
    | err => Belt.Result.Error("Unknown tag value found '" ++ (err ++ "'."))
    | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error(message)
    }
}

let () = {
  AesonSpec.goldenSpec(
    Test.decodePerson,
    Test.encodePerson,
    "person",
    "__tests__/golden/Person.json",
  )

  AesonSpec.goldenDirSpec(Test.decodePerson, Test.encodePerson, "person", "__tests__/golden/Person")

  AesonSpec.goldenDirSpec(
    Test.decodeCompany,
    Test.encodeCompany,
    "company",
    "__tests__/golden/Company",
  )

  AesonSpec.goldenDirSpec(Test.decodeShape, Test.encodeShape, "shape", "__tests__/golden/Shape")

  AesonSpec.goldenDirSpecWithEncoding(
    Test.decodePerson,
    Test.encodePerson,
    "person",
    "__tests__/golden/Person",
    #latin1,
  )

  AesonSpec.goldenDirSpecWithEncoding(
    Test.decodeCompany,
    Test.encodeCompany,
    "company",
    "__tests__/golden/Company",
    #latin1,
  )

  AesonSpec.goldenDirSpecWithEncoding(
    Test.decodeShape,
    Test.encodeShape,
    "shape",
    "__tests__/golden/Shape",
    #latin1,
  )

  /* these two tests should fail */
  /* AesonSpec.goldenDirSpec Test.decodePerson Test.brokenEncodePerson "person" "__tests__/golden/Person";
   *
   * AesonSpec.goldenDirSpec Test.decodeShape Test.encodeShape "shape" "__tests__/golden/Company"; */

  describe("isJsonFile", () =>
    test("", () => {
      let files = Node.Fs.readdirSync("__tests__/golden/Person")

      let filename = files[0]

      expect(AesonSpec.isJsonFile(filename)) |> toEqual(true)
    })
  )
}
