'use strict';

var Block        = require("bs-platform/lib/js/block.js");
var Js_exn       = require("bs-platform/lib/js/js_exn.js");
var AesonSpec    = require("../src/AesonSpec.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");

function encodePerson(p) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "name",
                p[/* name */0]
              ],
              /* :: */[
                /* tuple */[
                  "age",
                  p[/* age */1]
                ],
                /* [] */0
              ]
            ]);
}

function brokenEncodePerson(p) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "Name",
                p[/* name */0]
              ],
              /* :: */[
                /* tuple */[
                  "Age",
                  p[/* age */1]
                ],
                /* [] */0
              ]
            ]);
}

function decodePerson(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* name */Aeson_decode.field("name", Aeson_decode.string, json),
      /* age */Aeson_decode.field("age", Aeson_decode.$$int, json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodePerson: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

function encodeCompany(p) {
  return Aeson_encode.object_(/* :: */[
              /* tuple */[
                "companyName",
                p[/* companyName */0]
              ],
              /* :: */[
                /* tuple */[
                  "employees",
                  Aeson_encode.list(encodePerson, p[/* employees */1])
                ],
                /* [] */0
              ]
            ]);
}

function decodeCompany(json) {
  var exit = 0;
  var v;
  try {
    v = /* record */[
      /* companyName */Aeson_decode.field("companyName", Aeson_decode.string, json),
      /* employees */Aeson_decode.field("employees", (function (param) {
              return Aeson_decode.list((function (a) {
                            return Aeson_decode.unwrapResult(decodePerson(a));
                          }), param);
            }), json)
    ];
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, ["decodePerson: " + exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Ok */Block.__(0, [v]);
  }
  
}

var Test = /* module */[
  /* encodePerson */encodePerson,
  /* brokenEncodePerson */brokenEncodePerson,
  /* decodePerson */decodePerson,
  /* encodeCompany */encodeCompany,
  /* decodeCompany */decodeCompany
];

AesonSpec.file_roundtrip(decodePerson, encodePerson, "person", "__tests__/person.json");

AesonSpec.sample_roundtrip(decodePerson, encodePerson, "person", "__tests__/sample.json");

exports.Test = Test;
/*  Not a pure module */
