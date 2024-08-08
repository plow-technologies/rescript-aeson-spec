open Jest
open Expect

/* types */

type sample<'a> = {
  seed: float,
  samples: list<'a>,
}

let decodeSampleUnsafe = (decode, json) => {
  seed: Aeson.Decode.field("seed", Aeson.Decode.float, json),
  samples: Aeson.Decode.field(
    "samples",
    x => Aeson.Decode.list(a => Aeson.Decode.unwrapResult(decode(a)), x),
    json,
  ),
}

let decodeSample = (decode, json) =>
  switch decodeSampleUnsafe(decode, json) {
  | v => Ok(v)
  | exception Aeson.Decode.DecodeError(message) => Error("decodeSample: " ++ message)
  }

let encodeSample = (encode, sample) =>
  Aeson.Encode.object_(list{
    ("seed", Aeson.Encode.float(sample.seed)),
    ("samples", Aeson.Encode.list(encode, sample.samples)),
  })

/* internal functions */

let resultMap = (f, r) =>
  switch r {
  | Ok(a) => Ok(f(a))
  | Error(b) => Error(b)
  }

let getJsonSamples = json =>
  switch Js.Json.decodeObject(json) {
  | Some(dict) =>
    switch Js_dict.get(dict, "samples") {
    | Some(keyValue) => Js.Json.decodeArray(keyValue)
    | _ => None
    }
  | _ => None
  }

/* external functions */

/* roundtrip spec : given an object 'o', encode 'o' then decode the result, the decoded result must equal 'o'. */

let jsonRoundtripSpec = (decode, encode, json) => {
  let rDecoded = decode(json)
  expect(resultMap(encode, rDecoded))->toEqual(Ok(json))
}

let sampleJsonRoundtripSpec = (decode, encode, json) => {
  let rDecoded = decodeSample(decode, json)
  switch rDecoded {
  | Ok(decoded) =>
    let encoded = encodeSample(encode, decoded)
    let a = getJsonSamples(encoded)
    let b = getJsonSamples(json)
    switch (a, b) {
    | (Some(c), Some(d)) => Belt.Array.zip(c, d)->Array.map(pair => Ok(pair))->List.fromArray
    | _ =>
      list{
        Error(
          "Did not find key 'samples'. Are you using a JSON file produced by hspec-golden-aeson?",
        ),
      }
    }
  | Error(msg) =>
    list{
      Error(
        "Unable to decode golden file. Make sure the decode function matches the shape of the JSON file. Details: " ++
        msg,
      ),
    }
  }
}

let valueRoundtripSpec = (decode, encode, value) =>
  expect(decode(encode(value)))->toEqual(Ok(value))

/* file tests */

let goldenSpec = (decode, encode, name_of_type, json_file) =>
  describe(
    "AesonSpec.goldenSpec: " ++ (name_of_type ++ (" from file '" ++ (json_file ++ "'"))),
    () => {
      let json = Js.Json.parseExn(
        NodeJs.Fs.readFileSync(json_file)->NodeJs.Buffer.toStringWithEncoding(
          NodeJs.StringEncoding.utf8,
        ),
      )
      test("decode then encode: " ++ Js.Json.stringify(json), () =>
        jsonRoundtripSpec(decode, encode, json)
      )
    },
  )

let sampleGoldenSpec = (decode, encode, name_of_type, json_file) =>
  describe(
    "AesonSpec.sampleGoldenSpec: " ++
    (name_of_type ++
    (" from file '" ++ (json_file ++ "' with encoding utf8"))),
    () => {
      let json = Js.Json.parseExn(
        NodeJs.Fs.readFileSync(json_file)->NodeJs.Buffer.toStringWithEncoding(
          NodeJs.StringEncoding.utf8,
        ),
      )
      testAll(
        "decode then encode json_file",
        sampleJsonRoundtripSpec(decode, encode, json),
        result => {
          switch result {
          | Error(msg) => fail(msg)
          | Ok((x, y)) => expect(x)->toEqual(y)
          }
        },
      )
    },
  )

let encodingToString = encoding =>
  switch encoding {
  | #hex => "hex"
  | #utf8 => "utf8"
  | #ascii => "ascii"
  | #latin1 => "latin1"
  | #base64 => "base64"
  | #ucs2 => "ucs2"
  | #binary => "binary"
  | #utf16le => "utf16le"
  }

let sampleGoldenSpecWithEncoding = (decode, encode, name_of_type, json_file, encoding) =>
  describe(
    "AesonSpec.sampleGoldenSpec: " ++
    (name_of_type ++
    (" from file '" ++ (json_file ++ ("' with encoding " ++ encodingToString(encoding))))),
    () => {
      let json = Js.Json.parseExn(
        NodeJs.Fs.readFileSync(json_file)->NodeJs.Buffer.toStringWithEncoding(
          NodeJs.StringEncoding.utf8,
        ),
      )
      testAll(
        "decode then encode json_file",
        sampleJsonRoundtripSpec(decode, encode, json),
        result => {
          switch result {
          | Error(msg) => fail(msg)
          | Ok((x, y)) => expect(x)->toEqual(y)
          }
        },
      )
    },
  )

let isJsonFile = fileName => {
  let items = List.fromArray(Js.String.split(".", fileName))
  let length = Js.List.length(items)
  switch Js.List.nth(items, length - 1) {
  | Some(ext) => ext === "json"
  | None => false
  }
}

/* run roundtrip file test on a directory */
let goldenDirSpec = (decode, encode, name_of_type, json_dir) => {
  let files_in_dir = Js.Array.filter(isJsonFile, NodeJs.Fs.readdirSync(json_dir))
  Array.forEach(files_in_dir, json_file =>
    sampleGoldenSpec(decode, encode, name_of_type, json_dir ++ ("/" ++ json_file))
  )
}

/* run roundtrip file test on a directory */
let goldenDirSpecWithEncoding = (decode, encode, name_of_type, json_dir, encoding) => {
  let files_in_dir = Js.Array.filter(isJsonFile, NodeJs.Fs.readdirSync(json_dir))
  Array.forEach(files_in_dir, json_file =>
    sampleGoldenSpecWithEncoding(
      decode,
      encode,
      name_of_type,
      json_dir ++ ("/" ++ json_file),
      encoding,
    )
  )
}

let decodeIntWithResult = json => Aeson.Decode.wrapResult(Aeson.Decode.int, json)
