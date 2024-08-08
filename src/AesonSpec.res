open AesonSpec_Jest
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
    Aeson.Decode.list(a => Aeson.Decode.unwrapResult(decode(a))),
    json,
  ),
}

let decodeSample = (decode, json) =>
  switch decodeSampleUnsafe(decode, json) {
  | v => Belt.Result.Ok(v)
  | exception Aeson.Decode.DecodeError(message) => Belt.Result.Error("decodeSample: " ++ message)
  }

let encodeSample = (encode, sample) =>
  Aeson.Encode.object_(list{
    ("seed", Aeson.Encode.float(sample.seed)),
    ("samples", Aeson.Encode.list(encode, sample.samples)),
  })

/* internal functions */

let resultMap = (f, r) =>
  switch r {
  | Belt.Result.Ok(a) => Belt.Result.Ok(f(a))
  | Belt.Result.Error(b) => Belt.Result.Error(b)
  }

let isFail = x =>
  switch x {
  | Ok => false
  | _ => true
  }

let getFirstFail = xs => {
  let ys = List.fold_left((a, b) =>
    if isFail(b) {
      \"@"(a, list{b})
    } else {
      a
    }
  , list{}, xs)
  Belt.List.head(ys)
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
  expect(resultMap(encode, rDecoded)) |> toEqual(Belt.Result.Ok(json))
}

let sampleJsonRoundtripSpec = (decode, encode, json) => {
  let rDecoded = decodeSample(decode, json)
  switch rDecoded {
  | Belt.Result.Ok(decoded) =>
    let encoded = encodeSample(encode, decoded)
    let a = getJsonSamples(encoded)
    let b = getJsonSamples(json)
    switch (a, b) {
    | (Some(c), Some(d)) =>
      let z = Belt.List.zip(Array.to_list(c), Array.to_list(d))
      let xs = List.map(((x, y)) => expect(x) |> toEqual(y), z)
      let os = getFirstFail(xs)
      switch os {
      | Some(s) => s
      | None => pass
      }
    | _ =>
      fail("Did not find key 'samples'. Are you using a JSON file produced by hspec-golden-aeson?")
    }
  | Belt.Result.Error(msg) =>
    fail(
      "Unable to decode golden file. Make sure the decode function matches the shape of the JSON file. Details: " ++
      msg,
    )
  }
}

let valueRoundtripSpec = (decode, encode, value) =>
  expect(decode(encode(value))) |> toEqual(Belt.Result.Ok(value))

/* file tests */

let goldenSpec = (decode, encode, name_of_type, json_file) =>
  describe(
    "AesonSpec.goldenSpec: " ++ (name_of_type ++ (" from file '" ++ (json_file ++ "'"))),
    () => {
      let json = Js.Json.parseExn(Node.Fs.readFileAsUtf8Sync(json_file))
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
      let json = Js.Json.parseExn(Node.Fs.readFileAsUtf8Sync(json_file))
      test("decode then encode json_file", () => sampleJsonRoundtripSpec(decode, encode, json))
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
      let json = Js.Json.parseExn(Node.Fs.readFileSync(json_file, encoding))
      test("decode then encode json_file", () => sampleJsonRoundtripSpec(decode, encode, json))
    },
  )

let isJsonFile = fileName => {
  let items = Array.to_list(Js.String.split(".", fileName))
  let length = Js.List.length(items)
  switch Js.List.nth(items, length - 1) {
  | Some(ext) => ext === "json"
  | None => false
  }
}

/* run roundtrip file test on a directory */
let goldenDirSpec = (decode, encode, name_of_type, json_dir) => {
  let files_in_dir = Js.Array.filter(isJsonFile, Node.Fs.readdirSync(json_dir))
  Array.iter(
    json_file => sampleGoldenSpec(decode, encode, name_of_type, json_dir ++ ("/" ++ json_file)),
    files_in_dir,
  )
}

/* run roundtrip file test on a directory */
let goldenDirSpecWithEncoding = (decode, encode, name_of_type, json_dir, encoding) => {
  let files_in_dir = Js.Array.filter(isJsonFile, Node.Fs.readdirSync(json_dir))
  Array.iter(
    json_file =>
      sampleGoldenSpecWithEncoding(
        decode,
        encode,
        name_of_type,
        json_dir ++ ("/" ++ json_file),
        encoding,
      ),
    files_in_dir,
  )
}

let decodeIntWithResult = json => Aeson.Decode.wrapResult(Aeson.Decode.int, json)
