'use strict';

var Fs = require("fs");
var Jest = require("@glennsl/bs-jest/lib/js/src/jest.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Js_list = require("bs-platform/lib/js/js_list.js");
var NodeFetch = require("node-fetch");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");
var Bs_node_fetch = require("bs-node-fetch/lib/js/src/bs_node_fetch.js");
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

function jsonRoundtripSpec(decode, encode, json) {
  var rDecoded = Curry._1(decode, json);
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]), Jest.Expect[/* expect */0](resultMap(encode, rDecoded)));
}

function sampleJsonRoundtripSpec(decode, encode, json) {
  var rDecoded = decodeSample(decode, json);
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]), Jest.Expect[/* expect */0](resultMap((function (encoded) {
                        return encodeSample(encode, encoded);
                      }), rDecoded)));
}

function valueRoundtripSpec(decode, encode, value) {
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [value]), Jest.Expect[/* expect */0](Curry._1(decode, Curry._1(encode, value))));
}

function goldenSpec(decode, encode, name_of_type, json_file) {
  return Jest.describe("AesonSpec.goldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function (param) {
                var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
                return Jest.test("decode then encode: " + JSON.stringify(json), (function (param) {
                              return jsonRoundtripSpec(decode, encode, json);
                            }));
              }));
}

function sampleGoldenSpec(decode, encode, name_of_type, json_file) {
  return Jest.describe("AesonSpec.sampleGoldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function (param) {
                var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
                return Jest.test("decode then encode json_file", (function (param) {
                              return sampleJsonRoundtripSpec(decode, encode, json);
                            }));
              }));
}

function serverSpec(decode, encode, name_of_type, url, value) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var encodedString = JSON.stringify(Curry._1(encode, value));
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Post */2, Caml_option.some(headers), Caml_option.some(encodedString), undefined, undefined, /* CORS */3, undefined, undefined, undefined, undefined, undefined, undefined)(/* () */0);
  return Jest.describe("AesonSpec.serverSpec: " + name_of_type, (function (param) {
                return Jest.testPromise("encode, POST to server, receieve from server, decode: " + encodedString, undefined, (function (param) {
                              return NodeFetch.default(url, reqInit).then((function (response) {
                                            return response.text().then((function (text) {
                                                          return Promise.resolve(Jest.Expect[/* toEqual */12](value, Jest.Expect[/* expect */0](Aeson_decode.unwrapResult(Curry._1(decode, JSON.parse(text))))));
                                                        }));
                                          }));
                            }));
              }));
}

function sampleServerSpec(decode, encode, name_of_type, url, values) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var encodedString = JSON.stringify(Aeson_encode.list(encode, values));
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Post */2, Caml_option.some(headers), Caml_option.some(encodedString), undefined, undefined, /* CORS */3, undefined, undefined, undefined, undefined, undefined, undefined)(/* () */0);
  return Jest.describe("AesonSpec.sampleServerSpec: " + name_of_type, (function (param) {
                return Jest.testPromise("encode json_file, POST encoded to server, receieve response from server, decode response", undefined, (function (param) {
                              return NodeFetch.default(url, reqInit).then((function (response) {
                                            return response.text().then((function (text) {
                                                          return Promise.resolve(Jest.Expect[/* toEqual */12](values, Jest.Expect[/* expect */0](Aeson_decode.list((function (a) {
                                                                                    return Aeson_decode.unwrapResult(Curry._1(decode, a));
                                                                                  }), JSON.parse(text)))));
                                                        }));
                                          }));
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

function sampleGoldenAndServerSpec(decode, encode, name_of_type, url, json_dir) {
  var filesInDir = Fs.readdirSync(json_dir).filter(isJsonFile);
  console.log(filesInDir);
  return $$Array.iter((function (json_file) {
                var decode$1 = decode;
                var encode$1 = encode;
                var name_of_type$1 = name_of_type;
                var url$1 = url;
                var json_file$1 = json_dir + ("/" + json_file);
                var json = JSON.parse(Fs.readFileSync(json_file$1, "utf8"));
                var match = decodeSample(decode$1, json);
                if (match.tag) {
                  var message = match[0];
                  return Jest.describe("", (function (param) {
                                return Jest.test("", (function (param) {
                                              return Jest.fail(message);
                                            }));
                              }));
                } else {
                  var sample = match[0];
                  return Jest.describe("AesonSpec.sampleGoldenAndServerSpec: " + (name_of_type$1 + (" from file '" + (json_file$1 + "'"))), (function (param) {
                                Jest.test("decode then encode json_file", (function (param) {
                                        return Jest.Expect[/* toEqual */12](json, Jest.Expect[/* expect */0](encodeSample(encode$1, sample)));
                                      }));
                                var headers = Js_dict.fromList(/* :: */[
                                      /* tuple */[
                                        "Content-Type",
                                        "application/json"
                                      ],
                                      /* [] */0
                                    ]);
                                var encodedString = JSON.stringify(Aeson_encode.list(encode$1, sample[/* samples */1]));
                                var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Post */2, Caml_option.some(headers), Caml_option.some(encodedString), undefined, undefined, /* CORS */3, undefined, undefined, undefined, undefined, undefined, undefined)(/* () */0);
                                return Jest.testPromise("encode json_file, POST encoded to server, receieve response from server, decode response", undefined, (function (param) {
                                              return NodeFetch.default(url$1, reqInit).then((function (response) {
                                                            return response.text().then((function (text) {
                                                                          return Promise.resolve(Jest.Expect[/* toEqual */12](sample[/* samples */1], Jest.Expect[/* expect */0](Aeson_decode.list((function (a) {
                                                                                                    return Aeson_decode.unwrapResult(Curry._1(decode$1, a));
                                                                                                  }), JSON.parse(text)))));
                                                                        }));
                                                          }));
                                            }));
                              }));
                }
              }), filesInDir);
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
exports.serverSpec = serverSpec;
exports.sampleServerSpec = sampleServerSpec;
exports.isJsonFile = isJsonFile;
exports.goldenDirSpec = goldenDirSpec;
exports.sampleGoldenAndServerSpec = sampleGoldenAndServerSpec;
exports.decodeIntWithResult = decodeIntWithResult;
/* fs Not a pure module */
