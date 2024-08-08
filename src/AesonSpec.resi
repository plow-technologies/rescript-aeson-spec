type sample<'a> = {
  seed: float,
  samples: list<'a>,
}

let decodeSampleUnsafe: (Js_json.t => Belt.Result.t<'a, string>, Js_json.t) => sample<'a>

@@ocaml.text(" decode sample from a JSON but throw an exception if it is unable to decode ")

let decodeSample: (
  Js_json.t => Belt.Result.t<'a, string>,
  Js_json.t,
) => Belt.Result.t<sample<'a>, string>

@@ocaml.text(" decode sample from a JSON and return it as Belt.Result ")

let encodeSample: ('a => Js_json.t, sample<'a>) => Js_json.t

@@ocaml.text(" encode sample into a JSON ")

/* specs */

let jsonRoundtripSpec: (
  Js_json.t => Belt.Result.t<'a, string>,
  'a => Js_json.t,
  Js_json.t,
) => AesonSpec_Jest.assertion

@@ocaml.text(" try to encode and decode a JSON ")

let sampleJsonRoundtripSpec: (
  Js_json.t => Belt.Result.t<'a, string>,
  'a => Js_json.t,
  Js_json.t,
) => AesonSpec_Jest.assertion

@@ocaml.text(" try to encode and decode a sample JSON of a type ")

let valueRoundtripSpec: (
  Js_json.t => Belt.Result.t<'a, string>,
  'a => Js_json.t,
  'a,
) => AesonSpec_Jest.assertion

@@ocaml.text(" try to encode and decode a value of a type ")

let goldenSpec: (Js_json.t => Belt.Result.t<'a, string>, 'a => Js_json.t, string, string) => unit

@@ocaml.text(" decode and encode a golden file ")

let sampleGoldenSpec: (
  Js_json.t => Belt.Result.t<'a, string>,
  'a => Js_json.t,
  string,
  string,
) => unit

@@ocaml.text(
  " encode a sample of a type, POST it to a test server, receive a response and decode the response "
)

let isJsonFile: string => bool

@@ocaml.text(" test if file name ends in \".json\" ")

let goldenDirSpec: (Js_json.t => Belt.Result.t<'a, string>, 'a => Js_json.t, string, string) => unit

let goldenDirSpecWithEncoding: (
  Js_json.t => Belt.Result.t<'a, string>,
  'a => Js_json.t,
  string,
  string,
  Node.Fs.encoding,
) => unit

@@ocaml.text(" goldenSpec and sampleServerSpec ")

let decodeIntWithResult: Js_json.t => Belt.Result.t<int, string>
