'use strict';

var Fs            = require("fs");
var Jest          = require("bs-jest/lib/js/src/jest.js");
var Block         = require("bs-platform/lib/js/block.js");
var Curry         = require("bs-platform/lib/js/curry.js");
var Js_exn        = require("bs-platform/lib/js/js_exn.js");
var Js_dict       = require("bs-platform/lib/js/js_dict.js");
var FetchWithInit = require("node-fetch");
var Aeson_decode  = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode  = require("bs-aeson/lib/js/src/Aeson_encode.js");
var Bs_node_fetch = require("bs-node-fetch/lib/js/src/bs_node_fetch.js");

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
    var exn = Js_exn.internalToOCamlException(raw_exn);
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

function jsonRoundtripSingleSpec(decode, encode, json) {
  var rDecoded = Curry._1(decode, json);
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]))(Jest.Expect[/* expect */0](resultMap(encode, rDecoded)));
}

function jsonRoundtripSpec(decode, encode, json) {
  var rDecoded = decodeSample(decode, json);
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]))(Jest.Expect[/* expect */0](resultMap((function (encoded) {
                        return encodeSample(encode, encoded);
                      }), rDecoded)));
}

function valueRoundtripSpec(decode, encode, value) {
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [value]))(Jest.Expect[/* expect */0](Curry._1(decode, Curry._1(encode, value))));
}

function goldenSingleSpec(decode, encode, name_of_type, json_file) {
  describe("AesonSpec.goldenSingleSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function () {
          var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
          return Jest.test("decode then encode: " + JSON.stringify(json), (function () {
                        return jsonRoundtripSingleSpec(decode, encode, json);
                      }));
        }));
  return /* () */0;
}

function goldenSpec(decode, encode, name_of_type, json_file) {
  describe("AesonSpec.goldenSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function () {
          var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
          return Jest.test("decode then encode: " + JSON.stringify(json), (function () {
                        return jsonRoundtripSpec(decode, encode, json);
                      }));
        }));
  return /* () */0;
}

function serverSingleSpec(decode, encode, name_of_type, url, value) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var encodedString = JSON.stringify(Curry._1(encode, value));
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[headers], /* Some */[encodedString], /* None */0, /* None */0, /* Some */[/* CORS */3], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0);
  describe("AesonSpec.serverSpec: " + name_of_type, (function () {
          return Jest.testPromise("encode, POST to server, receieve from server, decode: " + encodedString, (function () {
                        return FetchWithInit(url, reqInit).then((function (response) {
                                      return response.text().then((function (text) {
                                                    return Promise.resolve(Jest.Expect[/* toEqual */12](value)(Jest.Expect[/* expect */0](Aeson_decode.unwrapResult(Curry._1(decode, JSON.parse(text))))));
                                                  }));
                                    }));
                      }));
        }));
  return /* () */0;
}

function serverSpec(decode, encode, name_of_type, url, values) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var encodedString = JSON.stringify(Aeson_encode.list(encode, values));
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[headers], /* Some */[encodedString], /* None */0, /* None */0, /* Some */[/* CORS */3], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0);
  describe("AesonSpec.serverSpec: " + name_of_type, (function () {
          return Jest.testPromise("encode, POST to server, receieve from server, decode: " + encodedString, (function () {
                        return FetchWithInit(url, reqInit).then((function (response) {
                                      return response.text().then((function (text) {
                                                    return Promise.resolve(Jest.Expect[/* toEqual */12](values)(Jest.Expect[/* expect */0](Aeson_decode.list((function (a) {
                                                                              return Aeson_decode.unwrapResult(Curry._1(decode, a));
                                                                            }), JSON.parse(text)))));
                                                  }));
                                    }));
                      }));
        }));
  return /* () */0;
}

function goldenAndServerSpec(decode, encode, name_of_type, url, json_file) {
  var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
  var match = decodeSample(decode, json);
  if (match.tag) {
    var message = match[0];
    describe("", (function () {
            return Jest.test("", (function () {
                          return Jest.fail(message);
                        }));
          }));
    return /* () */0;
  } else {
    var sample = match[0];
    describe("AesonSpec.goldenAndServerSpec: " + (name_of_type + (" from file '" + (json_file + "'"))), (function () {
            Jest.test("decode then encode: " + JSON.stringify(json), (function () {
                    return Jest.Expect[/* toEqual */12](json)(Jest.Expect[/* expect */0](encodeSample(encode, sample)));
                  }));
            var headers = Js_dict.fromList(/* :: */[
                  /* tuple */[
                    "Content-Type",
                    "application/json"
                  ],
                  /* [] */0
                ]);
            var encodedString = JSON.stringify(Aeson_encode.list(encode, sample[/* samples */1]));
            var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[headers], /* Some */[encodedString], /* None */0, /* None */0, /* Some */[/* CORS */3], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0);
            return Jest.testPromise("encode, POST to server, receieve from server, decode: " + encodedString, (function () {
                          return FetchWithInit(url, reqInit).then((function (response) {
                                        return response.text().then((function (text) {
                                                      return Promise.resolve(Jest.Expect[/* toEqual */12](sample[/* samples */1])(Jest.Expect[/* expect */0](Aeson_decode.list((function (a) {
                                                                                return Aeson_decode.unwrapResult(Curry._1(decode, a));
                                                                              }), JSON.parse(text)))));
                                                    }));
                                      }));
                        }));
          }));
    return /* () */0;
  }
}

exports.decodeSampleUnsafe      = decodeSampleUnsafe;
exports.decodeSample            = decodeSample;
exports.encodeSample            = encodeSample;
exports.jsonRoundtripSingleSpec = jsonRoundtripSingleSpec;
exports.jsonRoundtripSpec       = jsonRoundtripSpec;
exports.valueRoundtripSpec      = valueRoundtripSpec;
exports.goldenSingleSpec        = goldenSingleSpec;
exports.goldenSpec              = goldenSpec;
exports.serverSingleSpec        = serverSingleSpec;
exports.serverSpec              = serverSpec;
exports.goldenAndServerSpec     = goldenAndServerSpec;
/* fs Not a pure module */
