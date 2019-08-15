'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Js_json = require("bs-platform/lib/js/js_json.js");
var Js_list = require("bs-platform/lib/js/js_list.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");
var AesonSpec_Jest = require("./AesonSpec_Jest.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function decodeSampleUnsafe(decode, json) {
  return /* record */[
          /* seed */Aeson_decode.field("seed", Aeson_decode.$$float, json),
          /* samples */Aeson_decode.field("samples", (function (param) {
                  return Aeson_decode.list((function (a) {
                                return Aeson_decode.unwrapResult(Curry._1(decode, a));
                              }), param);
                }), json)
        ];
}

function decodeSample(decode, json) {
  var exit = 0;
  var v;
  try {
    v = decodeSampleUnsafe(decode, json);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodeSample: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function encodeSample(encode, sample) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "seed",
                sample[/* seed */0]
              ],
              /* :: */[
                /* tuple */[
                  "samples",
                  Aeson_encode.list(encode, sample[/* samples */1])
                ],
                /* [] */0
              ]
            ]);
}

function resultMap(f, r) {
  if (r.tag) {
    return /* Error */Block.__(1, [r[0]]);
  } else {
    return /* Ok */Block.__(0, [Curry._1(f, r[0])]);
  }
}

function isFail(x) {
  if (typeof x === "number") {
    return false;
  } else {
    return true;
  }
}

function getFirstFail(xs) {
  return Belt_List.head(List.fold_left((function (a, b) {
                    if (isFail(b)) {
                      return Pervasives.$at(a, /* :: */[
                                  b,
                                  /* [] */0
                                ]);
                    } else {
                      return a;
                    }
                  }), /* [] */0, xs));
}

function getJsonSamples(json) {
  var match = Js_json.decodeObject(json);
  if (match !== undefined) {
    var match$1 = Js_dict.get(Caml_option.valFromOption(match), "samples");
    if (match$1 !== undefined) {
      return Js_json.decodeArray(Caml_option.valFromOption(match$1));
    } else {
      return undefined;
    }
  }
  
}

function jsonRoundtripSpec(decode, encode, json) {
  var rDecoded = Curry._1(decode, json);
  return AesonSpec_Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]), AesonSpec_Jest.Expect[/* expect */0](resultMap(encode, rDecoded)));
}

function sampleJsonRoundtripSpec(decode, encode, json) {
  var rDecoded = decodeSample(decode, json);
  if (rDecoded.tag) {
    return AesonSpec_Jest.fail("Unable to decode golden file.");
  } else {
    var encoded = encodeSample(encode, rDecoded[0]);
    var a = getJsonSamples(encoded);
    var b = getJsonSamples(json);
    if (a !== undefined && b !== undefined) {
      var z = Belt_List.zip($$Array.to_list(a), $$Array.to_list(b));
      var xs = List.map((function (param) {
              return AesonSpec_Jest.Expect[/* toEqual */12](param[1], AesonSpec_Jest.Expect[/* expect */0](param[0]));
            }), z);
      var os = getFirstFail(xs);
      if (os !== undefined) {
        return os;
      } else {
        return AesonSpec_Jest.pass;
      }
    } else {
      return AesonSpec_Jest.fail("Did not find key 'samples'.");
    }
  }
}

function valueRoundtripSpec(decode, encode, value) {
  return AesonSpec_Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [value]), AesonSpec_Jest.Expect[/* expect */0](Curry._1(decode, Curry._1(encode, value))));
}

function goldenSpec(decode, encode, name_of_type, json_file) {
  return AesonSpec_Jest.describe("AesonSpec.goldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function (param) {
                var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
                return AesonSpec_Jest.test("decode then encode: " + JSON.stringify(json), (function (param) {
                              return jsonRoundtripSpec(decode, encode, json);
                            }));
              }));
}

function sampleGoldenSpec(decode, encode, name_of_type, json_file) {
  return AesonSpec_Jest.describe("AesonSpec.sampleGoldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function (param) {
                var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
                return AesonSpec_Jest.test("decode then encode json_file", (function (param) {
                              return sampleJsonRoundtripSpec(decode, encode, json);
                            }));
              }));
}

function isJsonFile(fileName) {
  var items = $$Array.to_list(fileName.split("."));
  var length = Js_list.length(items);
  var match = Js_list.nth(items, length - 1 | 0);
  if (match !== undefined) {
    return match === "json";
  } else {
    return false;
  }
}

function goldenDirSpec(decode, encode, name_of_type, json_dir) {
  var files_in_dir = Fs.readdirSync(json_dir).filter(isJsonFile);
  return $$Array.iter((function (json_file) {
                return sampleGoldenSpec(decode, encode, name_of_type, json_dir + ("/" + json_file));
              }), files_in_dir);
}

function decodeIntWithResult(json) {
  return Aeson_decode.wrapResult(Aeson_decode.$$int, json);
}

exports.decodeSampleUnsafe = decodeSampleUnsafe;
exports.decodeSample = decodeSample;
exports.encodeSample = encodeSample;
exports.jsonRoundtripSpec = jsonRoundtripSpec;
exports.sampleJsonRoundtripSpec = sampleJsonRoundtripSpec;
exports.valueRoundtripSpec = valueRoundtripSpec;
exports.goldenSpec = goldenSpec;
exports.sampleGoldenSpec = sampleGoldenSpec;
exports.isJsonFile = isJsonFile;
exports.goldenDirSpec = goldenDirSpec;
exports.decodeIntWithResult = decodeIntWithResult;
/* fs Not a pure module */
