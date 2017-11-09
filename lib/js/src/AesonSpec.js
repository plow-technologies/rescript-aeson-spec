'use strict';

var Fs            = require("fs");
var Jest          = require("bs-jest/lib/js/src/jest.js");
var Block         = require("bs-platform/lib/js/block.js");
var Curry         = require("bs-platform/lib/js/curry.js");
var Js_dict       = require("bs-platform/lib/js/js_dict.js");
var FetchWithInit = require("node-fetch");
var Bs_node_fetch = require("bs-node-fetch/lib/js/src/bs_node_fetch.js");

function server_roundtrip(decode, encode, name_of_type, url, value_of_type) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var encodedString = JSON.stringify(Curry._1(encode, value_of_type));
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[headers], /* Some */[encodedString], /* None */0, /* None */0, /* Some */[/* CORS */3], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0);
  describe("AesonSpec.server_roundtrip: " + name_of_type, (function () {
          return Jest.testPromise("send to and receive from server: " + encodedString, (function () {
                        return FetchWithInit(url, reqInit).then((function (response) {
                                      return response.text().then((function (text) {
                                                    return Promise.resolve(Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [value_of_type]))(Jest.Expect[/* expect */0](Curry._1(decode, JSON.parse(text)))));
                                                  }));
                                    }));
                      }));
        }));
  return /* () */0;
}

function file_roundtrip(decode, encode, name_of_type, json_file) {
  var mapJsResult = function (f, r) {
    if (r.tag) {
      return /* Error */Block.__(1, [r[0]]);
    } else {
      return /* Ok */Block.__(0, [Curry._1(f, r[0])]);
    }
  };
  describe("AesonSpec.file_roundtrip: " + (name_of_type + (" from file '" + (json_file + "'"))), (function () {
          var json = JSON.parse(Fs.readFileSync(json_file, "utf8"));
          return Jest.test("decode then encode: " + JSON.stringify(json), (function () {
                        var rDecoded = Curry._1(decode, json);
                        return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [json]))(Jest.Expect[/* expect */0](mapJsResult(encode, rDecoded)));
                      }));
        }));
  return /* () */0;
}

exports.server_roundtrip = server_roundtrip;
exports.file_roundtrip   = file_roundtrip;
/* fs Not a pure module */
