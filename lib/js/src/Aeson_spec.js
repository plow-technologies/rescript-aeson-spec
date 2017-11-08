'use strict';

var Fs                      = require("fs");
var Jest                    = require("bs-jest/lib/js/src/jest.js");
var Block                   = require("bs-platform/lib/js/block.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Js_dict                 = require("bs-platform/lib/js/js_dict.js");
var Caml_obj                = require("bs-platform/lib/js/caml_obj.js");
var FetchWithInit           = require("node-fetch");
var Bs_node_fetch           = require("bs-node-fetch/lib/js/src/bs_node_fetch.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function server_roundtrip(x, url, decode, encode) {
  var headers = Js_dict.fromList(/* :: */[
        /* tuple */[
          "Content-Type",
          "application/json"
        ],
        /* [] */0
      ]);
  var reqInit = Bs_node_fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[headers], /* Some */[JSON.stringify(Curry._1(encode, x))], /* None */0, /* None */0, /* Some */[/* CORS */3], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0);
  return FetchWithInit(url, reqInit).then((function (response) {
                return response.text().then((function (text) {
                              var tmp;
                              if (Caml_obj.caml_equal(Curry._1(decode, JSON.parse(text)), /* Ok */Block.__(0, [x]))) {
                                tmp = 0;
                              } else {
                                throw [
                                      Caml_builtin_exceptions.assert_failure,
                                      [
                                        "Aeson_spec.ml",
                                        20,
                                        26
                                      ]
                                    ];
                              }
                              return Promise.resolve(tmp);
                            }));
              }));
}

function file_roundtrip(typename, file, decode, encode) {
  var mapJsResult = function (f, r) {
    if (r.tag) {
      return /* Error */Block.__(1, [r[0]]);
    } else {
      return /* Ok */Block.__(0, [Curry._1(f, r[0])]);
    }
  };
  describe(typename, (function () {
          return Jest.test(typename, (function () {
                        var f = JSON.parse(Fs.readFileSync(file, "utf8"));
                        var rDecoded = Curry._1(decode, f);
                        return Jest.Expect[/* toEqual */12](/* Ok */Block.__(0, [f]))(Jest.Expect[/* expect */0](mapJsResult(encode, rDecoded)));
                      }));
        }));
  return /* () */0;
}

exports.server_roundtrip = server_roundtrip;
exports.file_roundtrip   = file_roundtrip;
/* fs Not a pure module */
