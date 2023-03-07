'use strict';

var Fs = require("fs");
var List = require("rescript/lib/js/list.js");
var $$Array = require("rescript/lib/js/array.js");
var Curry = require("rescript/lib/js/curry.js");
var Js_dict = require("rescript/lib/js/js_dict.js");
var Js_json = require("rescript/lib/js/js_json.js");
var Js_list = require("rescript/lib/js/js_list.js");
var Js_array = require("rescript/lib/js/js_array.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Js_string = require("rescript/lib/js/js_string.js");
var Pervasives = require("rescript/lib/js/pervasives.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");
var AesonSpec_Jest = require("./AesonSpec_Jest.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

function decodeSampleUnsafe(decode, json) {
  return {
          seed: Aeson_decode.field("seed", Aeson_decode.$$float, json),
          samples: Aeson_decode.field("samples", (function (param) {
                  return Aeson_decode.list((function (a) {
                                return Aeson_decode.unwrapResult(Curry._1(decode, a));
                              }), param);
                }), json)
        };
}

function decodeSample(decode, json) {
  var v;
  try {
    v = decodeSampleUnsafe(decode, json);
  }
  catch (raw_message){
    var message = Caml_js_exceptions.internalToOCamlException(raw_message);
    if (message.RE_EXN_ID === Aeson_decode.DecodeError) {
      return {
              TAG: /* Error */1,
              _0: "decodeSample: " + message._1
            };
    }
    throw message;
  }
  return {
          TAG: /* Ok */0,
          _0: v
        };
}

function encodeSample(encode, sample) {
  return Aeson_encode.object_({
              hd: [
                "seed",
                sample.seed
              ],
              tl: {
                hd: [
                  "samples",
                  Aeson_encode.list(encode, sample.samples)
                ],
                tl: /* [] */0
              }
            });
}

function resultMap(f, r) {
  if (r.TAG === /* Ok */0) {
    return {
            TAG: /* Ok */0,
            _0: Curry._1(f, r._0)
          };
  } else {
    return {
            TAG: /* Error */1,
            _0: r._0
          };
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
                      return Pervasives.$at(a, {
                                  hd: b,
                                  tl: /* [] */0
                                });
                    } else {
                      return a;
                    }
                  }), /* [] */0, xs));
}

function getJsonSamples(json) {
  var dict = Js_json.decodeObject(json);
  if (dict === undefined) {
    return ;
  }
  var keyValue = Js_dict.get(Caml_option.valFromOption(dict), "samples");
  if (keyValue !== undefined) {
    return Js_json.decodeArray(Caml_option.valFromOption(keyValue));
  }
  
}

function jsonRoundtripSpec(decode, encode, json) {
  var rDecoded = Curry._1(decode, json);
  return AesonSpec_Jest.Expect.toEqual({
              TAG: /* Ok */0,
              _0: json
            }, AesonSpec_Jest.Expect.expect(resultMap(encode, rDecoded)));
}

function sampleJsonRoundtripSpec(decode, encode, json) {
  var rDecoded = decodeSample(decode, json);
  if (rDecoded.TAG !== /* Ok */0) {
    return AesonSpec_Jest.fail("Unable to decode golden file. Make sure the decode function matches the shape of the JSON file. Details: " + rDecoded._0);
  }
  var encoded = encodeSample(encode, rDecoded._0);
  var a = getJsonSamples(encoded);
  var b = getJsonSamples(json);
  if (a === undefined) {
    return AesonSpec_Jest.fail("Did not find key 'samples'. Are you using a JSON file produced by hspec-golden-aeson?");
  }
  if (b === undefined) {
    return AesonSpec_Jest.fail("Did not find key 'samples'. Are you using a JSON file produced by hspec-golden-aeson?");
  }
  var z = Belt_List.zip($$Array.to_list(a), $$Array.to_list(b));
  var xs = List.map((function (param) {
          return AesonSpec_Jest.Expect.toEqual(param[1], AesonSpec_Jest.Expect.expect(param[0]));
        }), z);
  var os = getFirstFail(xs);
  if (os !== undefined) {
    return os;
  } else {
    return AesonSpec_Jest.pass;
  }
}

function valueRoundtripSpec(decode, encode, value) {
  return AesonSpec_Jest.Expect.toEqual({
              TAG: /* Ok */0,
              _0: value
            }, AesonSpec_Jest.Expect.expect(Curry._1(decode, Curry._1(encode, value))));
}

function goldenSpec(decode, encode, name_of_type, json_file) {
  AesonSpec_Jest.describe("AesonSpec.goldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function (param) {
          var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
          AesonSpec_Jest.test("decode then encode: " + JSON.stringify(json), (function (param) {
                  return jsonRoundtripSpec(decode, encode, json);
                }));
        }));
}

function sampleGoldenSpec(decode, encode, name_of_type, json_file) {
  AesonSpec_Jest.describe("AesonSpec.sampleGoldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function (param) {
          var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
          AesonSpec_Jest.test("decode then encode json_file", (function (param) {
                  return sampleJsonRoundtripSpec(decode, encode, json);
                }));
        }));
}

function isJsonFile(fileName) {
  var items = $$Array.to_list(Js_string.split(".", fileName));
  var length = Js_list.length(items);
  var ext = Js_list.nth(items, length - 1 | 0);
  if (ext !== undefined) {
    return ext === "json";
  } else {
    return false;
  }
}

function goldenDirSpec(decode, encode, name_of_type, json_dir) {
  var files_in_dir = Js_array.filter(isJsonFile, Fs.readdirSync(json_dir));
  $$Array.iter((function (json_file) {
          sampleGoldenSpec(decode, encode, name_of_type, json_dir + ("/" + json_file));
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
