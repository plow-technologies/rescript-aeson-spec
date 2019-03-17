'use strict';

var Fs = require("fs");
var Jest = require("@glennsl/bs-jest/lib/js/src/jest.js");
var Block = require("bs-platform/lib/js/block.js");
var Js_json = require("bs-platform/lib/js/js_json.js");
var AesonSpec = require("../src/AesonSpec.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

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
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
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
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
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

function encodeShape(x) {
  switch (x.tag | 0) {
    case 0 : 
        return Aeson_encode.object_(/* :: */[
                    /* tuple */[
                      "tag",
                      "Square"
                    ],
                    /* :: */[
                      /* tuple */[
                        "contents",
                        /* array */[
                          x[0],
                          x[1]
                        ]
                      ],
                      /* [] */0
                    ]
                  ]);
    case 1 : 
        return Aeson_encode.object_(/* :: */[
                    /* tuple */[
                      "tag",
                      "Triangle"
                    ],
                    /* :: */[
                      /* tuple */[
                        "contents",
                        /* array */[
                          x[0],
                          x[1],
                          x[2]
                        ]
                      ],
                      /* [] */0
                    ]
                  ]);
    case 2 : 
        return Aeson_encode.object_(/* :: */[
                    /* tuple */[
                      "tag",
                      "Rectangle"
                    ],
                    /* :: */[
                      /* tuple */[
                        "contents",
                        /* array */[
                          x[0],
                          x[1],
                          x[2],
                          x[3]
                        ]
                      ],
                      /* [] */0
                    ]
                  ]);
    
  }
}

function decodeShape(json) {
  var exit = 0;
  var err;
  try {
    err = Aeson_decode.field("tag", Aeson_decode.string, json);
    exit = 1;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Aeson_decode.DecodeError) {
      return /* Error */Block.__(1, [exn[1]]);
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    switch (err) {
      case "Rectangle" : 
          var match = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (match !== undefined) {
            var v = match;
            var exit$1 = 0;
            var v0;
            try {
              v0 = Aeson_decode.$$int(Caml_array.caml_array_get(v, 0));
              exit$1 = 2;
            }
            catch (raw_exn$1){
              var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
              if (exn$1[0] === Aeson_decode.DecodeError) {
                return /* Error */Block.__(1, ["Rectangle: " + exn$1[1]]);
              } else {
                throw exn$1;
              }
            }
            if (exit$1 === 2) {
              var exit$2 = 0;
              var v1;
              try {
                v1 = Aeson_decode.$$int(Caml_array.caml_array_get(v, 1));
                exit$2 = 3;
              }
              catch (raw_exn$2){
                var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$2);
                if (exn$2[0] === Aeson_decode.DecodeError) {
                  return /* Error */Block.__(1, ["Rectangle: " + exn$2[1]]);
                } else {
                  throw exn$2;
                }
              }
              if (exit$2 === 3) {
                var exit$3 = 0;
                var v2;
                try {
                  v2 = Aeson_decode.$$int(Caml_array.caml_array_get(v, 2));
                  exit$3 = 4;
                }
                catch (raw_exn$3){
                  var exn$3 = Caml_js_exceptions.internalToOCamlException(raw_exn$3);
                  if (exn$3[0] === Aeson_decode.DecodeError) {
                    return /* Error */Block.__(1, ["Rectangle: " + exn$3[1]]);
                  } else {
                    throw exn$3;
                  }
                }
                if (exit$3 === 4) {
                  var exit$4 = 0;
                  var v3;
                  try {
                    v3 = Aeson_decode.$$int(Caml_array.caml_array_get(v, 3));
                    exit$4 = 5;
                  }
                  catch (raw_exn$4){
                    var exn$4 = Caml_js_exceptions.internalToOCamlException(raw_exn$4);
                    if (exn$4[0] === Aeson_decode.DecodeError) {
                      return /* Error */Block.__(1, ["Rectangle: " + exn$4[1]]);
                    } else {
                      throw exn$4;
                    }
                  }
                  if (exit$4 === 5) {
                    return /* Ok */Block.__(0, [/* Rectangle */Block.__(2, [
                                  v0,
                                  v1,
                                  v2,
                                  v3
                                ])]);
                  }
                  
                }
                
              }
              
            }
            
          } else {
            return /* Error */Block.__(1, ["Rectangle expected an array."]);
          }
          break;
      case "Square" : 
          var match$1 = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (match$1 !== undefined) {
            var v$1 = match$1;
            var exit$5 = 0;
            var v0$1;
            try {
              v0$1 = Aeson_decode.$$int(Caml_array.caml_array_get(v$1, 0));
              exit$5 = 2;
            }
            catch (raw_exn$5){
              var exn$5 = Caml_js_exceptions.internalToOCamlException(raw_exn$5);
              if (exn$5[0] === Aeson_decode.DecodeError) {
                return /* Error */Block.__(1, ["Square: " + exn$5[1]]);
              } else {
                throw exn$5;
              }
            }
            if (exit$5 === 2) {
              var exit$6 = 0;
              var v1$1;
              try {
                v1$1 = Aeson_decode.$$int(Caml_array.caml_array_get(v$1, 1));
                exit$6 = 3;
              }
              catch (raw_exn$6){
                var exn$6 = Caml_js_exceptions.internalToOCamlException(raw_exn$6);
                if (exn$6[0] === Aeson_decode.DecodeError) {
                  return /* Error */Block.__(1, ["Square: " + exn$6[1]]);
                } else {
                  throw exn$6;
                }
              }
              if (exit$6 === 3) {
                return /* Ok */Block.__(0, [/* Square */Block.__(0, [
                              v0$1,
                              v1$1
                            ])]);
              }
              
            }
            
          } else {
            return /* Error */Block.__(1, ["Square expected an array."]);
          }
          break;
      case "Triangle" : 
          var match$2 = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (match$2 !== undefined) {
            var v$2 = match$2;
            var exit$7 = 0;
            var v0$2;
            try {
              v0$2 = Aeson_decode.$$int(Caml_array.caml_array_get(v$2, 0));
              exit$7 = 2;
            }
            catch (raw_exn$7){
              var exn$7 = Caml_js_exceptions.internalToOCamlException(raw_exn$7);
              if (exn$7[0] === Aeson_decode.DecodeError) {
                return /* Error */Block.__(1, ["Triangle: " + exn$7[1]]);
              } else {
                throw exn$7;
              }
            }
            if (exit$7 === 2) {
              var exit$8 = 0;
              var v1$2;
              try {
                v1$2 = Aeson_decode.$$int(Caml_array.caml_array_get(v$2, 1));
                exit$8 = 3;
              }
              catch (raw_exn$8){
                var exn$8 = Caml_js_exceptions.internalToOCamlException(raw_exn$8);
                if (exn$8[0] === Aeson_decode.DecodeError) {
                  return /* Error */Block.__(1, ["Triangle: " + exn$8[1]]);
                } else {
                  throw exn$8;
                }
              }
              if (exit$8 === 3) {
                var exit$9 = 0;
                var v2$1;
                try {
                  v2$1 = Aeson_decode.$$int(Caml_array.caml_array_get(v$2, 2));
                  exit$9 = 4;
                }
                catch (raw_exn$9){
                  var exn$9 = Caml_js_exceptions.internalToOCamlException(raw_exn$9);
                  if (exn$9[0] === Aeson_decode.DecodeError) {
                    return /* Error */Block.__(1, ["Triangle: " + exn$9[1]]);
                  } else {
                    throw exn$9;
                  }
                }
                if (exit$9 === 4) {
                  return /* Ok */Block.__(0, [/* Triangle */Block.__(1, [
                                v0$2,
                                v1$2,
                                v2$1
                              ])]);
                }
                
              }
              
            }
            
          } else {
            return /* Error */Block.__(1, ["Triangle expected an array."]);
          }
          break;
      default:
        return /* Error */Block.__(1, ["Unknown tag value found '" + (err + "'.")]);
    }
  }
  
}

var Test = /* module */[
  /* encodePerson */encodePerson,
  /* brokenEncodePerson */brokenEncodePerson,
  /* decodePerson */decodePerson,
  /* encodeCompany */encodeCompany,
  /* decodeCompany */decodeCompany,
  /* encodeShape */encodeShape,
  /* decodeShape */decodeShape
];

var person = /* record */[
  /* name */"Javier",
  /* age */50
];

var company_001 = /* employees : :: */[
  person,
  /* :: */[
    /* record */[
      /* name */"Joaquim",
      /* age */45
    ],
    /* :: */[
      /* record */[
        /* name */"Jordi",
        /* age */23
      ],
      /* [] */0
    ]
  ]
];

var company = /* record */[
  /* companyName */"Acme",
  company_001
];

AesonSpec.goldenSpec(decodePerson, encodePerson, "person", "__tests__/golden/Person.json");

AesonSpec.serverSpec(decodePerson, encodePerson, "person", "http://localhost:8081/person", person);

AesonSpec.serverSpec(decodeCompany, encodeCompany, "company", "http://localhost:8081/company", company);

AesonSpec.goldenDirSpec(decodePerson, encodePerson, "person", "__tests__/golden/Person");

AesonSpec.goldenDirSpec(decodeCompany, encodeCompany, "company", "__tests__/golden/Company");

AesonSpec.goldenDirSpec(decodeShape, encodeShape, "shape", "__tests__/golden/Shape");

AesonSpec.sampleGoldenAndServerSpec(decodePerson, encodePerson, "person", "http://localhost:8081/people", "__tests__/golden/Person");

AesonSpec.sampleGoldenAndServerSpec(decodeCompany, encodeCompany, "company", "http://localhost:8081/companies", "__tests__/golden/Company");

AesonSpec.sampleGoldenAndServerSpec(decodeShape, encodeShape, "shape", "http://localhost:8081/shapes", "__tests__/golden/Shape");

Jest.describe("isJsonFile", (function (param) {
        return Jest.test("", (function (param) {
                      var files = Fs.readdirSync("__tests__/golden/Person");
                      var filename = Caml_array.caml_array_get(files, 0);
                      return Jest.Expect[/* toEqual */12](true, Jest.Expect[/* expect */0](AesonSpec.isJsonFile(filename)));
                    }));
      }));

exports.Test = Test;
/*  Not a pure module */
