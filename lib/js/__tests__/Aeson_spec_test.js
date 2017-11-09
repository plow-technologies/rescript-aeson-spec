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

var Test = /* module */[
  /* encodePerson */encodePerson,
  /* brokenEncodePerson */brokenEncodePerson,
  /* decodePerson */decodePerson
];

AesonSpec.server_roundtrip("person", /* record */[
      /* name */"Javier",
      /* age */50
    ], "http://localhost:8081/person", decodePerson, encodePerson);

AesonSpec.file_roundtrip("person", "__tests__/person.json", decodePerson, encodePerson);

exports.Test = Test;
/*  Not a pure module */
