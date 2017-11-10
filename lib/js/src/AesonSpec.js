'use strict';

var Fs            = require("fs");
var Jest          = require("bs-jest/lib/js/src/jest.js");
var List          = require("bs-platform/lib/js/list.js");
var $$Array       = require("bs-platform/lib/js/array.js");
var Block         = require("bs-platform/lib/js/block.js");
var Curry         = require("bs-platform/lib/js/curry.js");
var Js_exn        = require("bs-platform/lib/js/js_exn.js");
var Js_dict       = require("bs-platform/lib/js/js_dict.js");
var Js_json       = require("bs-platform/lib/js/js_json.js");
var FetchWithInit = require("node-fetch");
var Aeson_decode  = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Bs_node_fetch = require("bs-node-fetch/lib/js/src/bs_node_fetch.js");

function decode_sample_unsafe(json) {
  var match = Aeson_decode.field("samples", Js_json.decodeArray, json);
  var tmp;
  if (match) {
    tmp = match[0];
  } else {
    throw [
          Aeson_decode.DecodeError,
          "invalid array"
        ];
  }
  return /* record */[
          /* seed */Aeson_decode.field("seed", Aeson_decode.$$float, json),
          /* samples */tmp
        ];
}

function decode_sample(json) {
  var exit = 0;
  var v;
  try {
    v = decode_sample_unsafe(json);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decode_sample: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function result_map(f, r) {
  if (r.tag) {
    return /* Error */Block.__(1, [r[0]]);
  } else {
    return /* Ok */Block.__(0, [Curry._1(f, r[0])]);
  }
}

function roundtrip(decode, encode, json) {
  var rDecoded = Curry._1(decode, json);
  return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]))(Jest.Expect[/* expect */0](result_map(encode, rDecoded)));
}

function server_test(decode, encode, _, url, value_of_type) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var encodedString = JSON.stringify(Curry._1(encode, value_of_type));
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[headers], /* Some */[encodedString], /* None */0, /* None */0, /* Some */[/* CORS */3], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0);
  return FetchWithInit(url, reqInit).then((function (response) {
                return response.text().then((function (text) {
                              console.log(text);
                              return Promise.resolve(Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [value_of_type]))(Jest.Expect[/* expect */0](Curry._1(decode, JSON.parse(text)))));
                            }));
              }));
}

function server_roundtrip(decode, encode, name_of_type, url, value_of_type) {
  var encodedString = JSON.stringify(Curry._1(encode, value_of_type));
  describe("AesonSpec.server_roundtrip: " + name_of_type, (function () {
          return Jest.testPromise("send to and receive from server: " + encodedString, (function () {
                        return server_test(decode, encode, name_of_type, url, value_of_type);
                      }));
        }));
  return /* () */0;
}

function server_roundtrip_set(decode, encode, name_of_type, url, values_of_type) {
  return List.iter((function (value_of_type) {
                return server_roundtrip(decode, encode, name_of_type, url, value_of_type);
              }), values_of_type);
}

function file_roundtrip(decode, encode, name_of_type, json_file) {
  describe("AesonSpec.file_roundtrip: " + (name_of_type + (" from file '" + (json_file + "'"))), (function () {
          var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
          return Jest.test("decode then encode: " + JSON.stringify(json), (function () {
                        return roundtrip(decode, encode, json);
                      }));
        }));
  return /* () */0;
}

function sample_roundtrip(decode, encode, name_of_type, json_file) {
  var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
  var match = decode_sample(json);
  if (match.tag) {
    var error = match[0];
    describe("", (function () {
            return Jest.test("", (function () {
                          return Jest.fail(error);
                        }));
          }));
    return /* () */0;
  } else {
    var sample = match[0];
    describe(name_of_type, (function () {
            return List.iter((function (sample) {
                          return Jest.test("samples", (function () {
                                        return roundtrip(decode, encode, sample);
                                      }));
                        }), $$Array.to_list(sample[/* samples */1]));
          }));
    return /* () */0;
  }
}

function golden(decode, encode, name_of_type, url, json_file) {
  var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
  var match = decode_sample(json);
  if (match.tag) {
    var error = match[0];
    describe("", (function () {
            return Jest.test("", (function () {
                          return Jest.fail(error);
                        }));
          }));
    return /* () */0;
  } else {
    var sample = match[0];
    describe("golden test for: " + name_of_type, (function () {
            return List.iter((function (sample) {
                          Jest.test("file", (function () {
                                  return roundtrip(decode, encode, sample);
                                }));
                          return Jest.testPromise("server", (function () {
                                        return server_test(decode, encode, name_of_type, url, Aeson_decode.unwrapResult(Curry._1(decode, sample)));
                                      }));
                        }), $$Array.to_list(sample[/* samples */1]));
          }));
    return /* () */0;
  }
}

exports.decode_sample_unsafe = decode_sample_unsafe;
exports.decode_sample        = decode_sample;
exports.server_roundtrip     = server_roundtrip;
exports.server_roundtrip_set = server_roundtrip_set;
exports.file_roundtrip       = file_roundtrip;
exports.sample_roundtrip     = sample_roundtrip;
exports.golden               = golden;
/* fs Not a pure module */
