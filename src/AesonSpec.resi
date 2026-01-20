type sample<'a> = {
  seed: float,
  samples: list<'a>,
}

let decodeSampleUnsafe: (JSON.t => result<'a, string>, JSON.t) => sample<'a>

@@ocaml.text(" decode sample from a JSON but throw an exception if it is unable to decode ")

let decodeSample: (JSON.t => result<'a, string>, JSON.t) => result<sample<'a>, string>

@@ocaml.text(" decode sample from a JSON and return it as Belt.Result ")

let encodeSample: ('a => JSON.t, sample<'a>) => JSON.t

@@ocaml.text(" encode sample into a JSON ")

/* specs */

let jsonRoundtripSpec: (JSON.t => result<'a, string>, 'a => JSON.t, JSON.t) => Jest.assertion

@@ocaml.text(" try to encode and decode a JSON ")

let sampleJsonRoundtripSpec: (
  JSON.t => result<'a, string>,
  'a => JSON.t,
  JSON.t,
) => list<result<(JSON.t, JSON.t), string>>

@@ocaml.text(" try to encode and decode a sample JSON of a type ")

let valueRoundtripSpec: (JSON.t => result<'a, string>, 'a => JSON.t, 'a) => Jest.assertion

@@ocaml.text(" try to encode and decode a value of a type ")

let goldenSpec: (JSON.t => result<'a, string>, 'a => JSON.t, string, string) => unit

@@ocaml.text(" decode and encode a golden file ")

let sampleGoldenSpec: (JSON.t => result<'a, string>, 'a => JSON.t, string, string) => unit

let sampleGoldenSpecWithEncoding: (
  JSON.t => result<'a, string>,
  Aeson.Encode.encoder<'a>,
  string,
  string,
  [< #ascii | #base64 | #binary | #hex | #latin1 | #ucs2 | #utf16le | #utf8],
) => unit

@@ocaml.text(
  " encode a sample of a type, POST it to a test server, receive a response and decode the response "
)

let isJsonFile: string => bool

@@ocaml.text(" test if file name ends in \".json\" ")

let goldenDirSpec: (JSON.t => result<'a, string>, 'a => JSON.t, string, string) => unit

let goldenDirSpecWithEncoding: (
  JSON.t => result<'a, string>,
  'a => JSON.t,
  string,
  string,
  [< #ascii | #base64 | #binary | #hex | #latin1 | #ucs2 | #utf16le | #utf8],
) => unit

@@ocaml.text(" goldenSpec and sampleServerSpec ")

let decodeIntWithResult: JSON.t => result<int, string>
