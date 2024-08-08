'use strict';

var Jest = require("@glennsl/rescript-jest/lib/js/src/jest.js");
var Js_dict = require("rescript/lib/js/js_dict.js");
var Js_json = require("rescript/lib/js/js_json.js");
var Js_list = require("rescript/lib/js/js_list.js");
var Nodefs = require("node:fs");
var Js_array = require("rescript/lib/js/js_array.js");
var Js_string = require("rescript/lib/js/js_string.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Core__List = require("@rescript/core/lib/js/src/Core__List.js");
var Aeson_decode = require("@plowtech/rescript-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("@plowtech/rescript-aeson/lib/js/src/Aeson_encode.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

function decodeSampleUnsafe(decode, json) {
  return {
          seed: Aeson_decode.field("seed", Aeson_decode.$$float, json),
          samples: Aeson_decode.field("samples", (function (x) {
                  return Aeson_decode.list((function (a) {
                                return Aeson_decode.unwrapResult(decode(a));
                              }), x);
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
              TAG: "Error",
              _0: "decodeSample: " + message._1
            };
    }
    throw message;
  }
  return {
          TAG: "Ok",
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
  if (r.TAG === "Ok") {
    return {
            TAG: "Ok",
            _0: f(r._0)
          };
  } else {
    return {
            TAG: "Error",
            _0: r._0
          };
  }
}

function getJsonSamples(json) {
  var dict = Js_json.decodeObject(json);
  if (dict === undefined) {
    return ;
  }
  var keyValue = Js_dict.get(dict, "samples");
  if (keyValue !== undefined) {
    return Js_json.decodeArray(keyValue);
  }
  
}

function jsonRoundtripSpec(decode, encode, json) {
  var rDecoded = decode(json);
  return Jest.Expect.toEqual(Jest.Expect.expect(resultMap(encode, rDecoded)), {
              TAG: "Ok",
              _0: json
            });
}

function sampleJsonRoundtripSpec(decode, encode, json) {
  var rDecoded = decodeSample(decode, json);
  if (rDecoded.TAG !== "Ok") {
    return {
            hd: {
              TAG: "Error",
              _0: "Unable to decode golden file. Make sure the decode function matches the shape of the JSON file. Details: " + rDecoded._0
            },
            tl: /* [] */0
          };
  }
  var encoded = encodeSample(encode, rDecoded._0);
  var a = getJsonSamples(encoded);
  var b = getJsonSamples(json);
  if (a !== undefined && b !== undefined) {
    return Core__List.fromArray(Belt_Array.zip(a, b).map(function (pair) {
                    return {
                            TAG: "Ok",
                            _0: pair
                          };
                  }));
  } else {
    return {
            hd: {
              TAG: "Error",
              _0: "Did not find key 'samples'. Are you using a JSON file produced by hspec-golden-aeson?"
            },
            tl: /* [] */0
          };
  }
}

function valueRoundtripSpec(decode, encode, value) {
  return Jest.Expect.toEqual(Jest.Expect.expect(decode(encode(value))), {
              TAG: "Ok",
              _0: value
            });
}

function goldenSpec(decode, encode, name_of_type, json_file) {
  Jest.describe("AesonSpec.goldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function () {
          var json = JSON.parse(Nodefs.readFileSync(json_file).toString("utf8"));
          Jest.test("decode then encode: " + JSON.stringify(json), (function () {
                  return jsonRoundtripSpec(decode, encode, json);
                }));
        }));
}

function sampleGoldenSpec(decode, encode, name_of_type, json_file) {
  Jest.describe("AesonSpec.sampleGoldenSpec: " + (name_of_type + (" from file '" + (json_file + "' with encoding utf8"))), (function () {
          var json = JSON.parse(Nodefs.readFileSync(json_file).toString("utf8"));
          Jest.testAll("decode then encode json_file", sampleJsonRoundtripSpec(decode, encode, json), (function (result) {
                  if (result.TAG !== "Ok") {
                    return Jest.fail(result._0);
                  }
                  var match = result._0;
                  return Jest.Expect.toEqual(Jest.Expect.expect(match[0]), match[1]);
                }));
        }));
}

function encodingToString(encoding) {
  if (encoding === "utf8") {
    return "utf8";
  } else if (encoding === "base64") {
    return "base64";
  } else if (encoding === "latin1") {
    return "latin1";
  } else if (encoding === "binary") {
    return "binary";
  } else if (encoding === "hex") {
    return "hex";
  } else if (encoding === "ascii") {
    return "ascii";
  } else if (encoding === "utf16le") {
    return "utf16le";
  } else {
    return "ucs2";
  }
}

function isJsonFile(fileName) {
  var items = Core__List.fromArray(Js_string.split(".", fileName));
  var length = Js_list.length(items);
  var ext = Js_list.nth(items, length - 1 | 0);
  if (ext !== undefined) {
    return ext === "json";
  } else {
    return false;
  }
}

function goldenDirSpec(decode, encode, name_of_type, json_dir) {
  var files_in_dir = Js_array.filter(isJsonFile, Nodefs.readdirSync(json_dir));
  files_in_dir.forEach(function (json_file) {
        sampleGoldenSpec(decode, encode, name_of_type, json_dir + ("/" + json_file));
      });
}

function goldenDirSpecWithEncoding(decode, encode, name_of_type, json_dir, encoding) {
  var files_in_dir = Js_array.filter(isJsonFile, Nodefs.readdirSync(json_dir));
  files_in_dir.forEach(function (json_file) {
        var json_file$1 = json_dir + ("/" + json_file);
        Jest.describe("AesonSpec.sampleGoldenSpec: " + (name_of_type + (" from file '" + (json_file$1 + ("' with encoding " + encodingToString(encoding))))), (function () {
                var json = JSON.parse(Nodefs.readFileSync(json_file$1).toString("utf8"));
                Jest.testAll("decode then encode json_file", sampleJsonRoundtripSpec(decode, encode, json), (function (result) {
                        if (result.TAG !== "Ok") {
                          return Jest.fail(result._0);
                        }
                        var match = result._0;
                        return Jest.Expect.toEqual(Jest.Expect.expect(match[0]), match[1]);
                      }));
              }));
      });
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
exports.goldenDirSpecWithEncoding = goldenDirSpecWithEncoding;
exports.decodeIntWithResult = decodeIntWithResult;
/* Jest Not a pure module */
