'use strict';

var Fs = require("fs");
var Js_json = require("bs-platform/lib/js/js_json.js");
var AesonSpec = require("../src/AesonSpec.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Aeson_decode = require("bs-aeson/lib/js/src/Aeson_decode.js");
var Aeson_encode = require("bs-aeson/lib/js/src/Aeson_encode.js");
var AesonSpec_Jest = require("../src/AesonSpec_Jest.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function encodePerson(p) {
  return Aeson_encode.object_({
              hd: [
                "name",
                p.name
              ],
              tl: {
                hd: [
                  "age",
                  p.age
                ],
                tl: /* [] */0
              }
            });
}

function brokenEncodePerson(p) {
  return Aeson_encode.object_({
              hd: [
                "Name",
                p.name
              ],
              tl: {
                hd: [
                  "Age",
                  p.age
                ],
                tl: /* [] */0
              }
            });
}

function decodePerson(json) {
  var v;
  try {
    v = {
      name: Aeson_decode.field("name", Aeson_decode.string, json),
      age: Aeson_decode.field("age", Aeson_decode.$$int, json)
    };
  }
  catch (raw_message){
    var message = Caml_js_exceptions.internalToOCamlException(raw_message);
    if (message.RE_EXN_ID === Aeson_decode.DecodeError) {
      return {
              TAG: /* Error */1,
              _0: "decodePerson: " + message._1
            };
    }
    throw message;
  }
  return {
          TAG: /* Ok */0,
          _0: v
        };
}

function brokenDecodePerson(json) {
  var v;
  try {
    v = {
      name: Aeson_decode.field("name!!!!", Aeson_decode.string, json),
      age: Aeson_decode.field("age!!!!!", Aeson_decode.$$int, json)
    };
  }
  catch (raw_message){
    var message = Caml_js_exceptions.internalToOCamlException(raw_message);
    if (message.RE_EXN_ID === Aeson_decode.DecodeError) {
      return {
              TAG: /* Error */1,
              _0: "decodePerson: " + message._1
            };
    }
    throw message;
  }
  return {
          TAG: /* Ok */0,
          _0: v
        };
}

function encodeCompany(p) {
  return Aeson_encode.object_({
              hd: [
                "companyName",
                p.companyName
              ],
              tl: {
                hd: [
                  "employees",
                  Aeson_encode.list(encodePerson, p.employees)
                ],
                tl: /* [] */0
              }
            });
}

function decodeCompany(json) {
  var v;
  try {
    v = {
      companyName: Aeson_decode.field("companyName", Aeson_decode.string, json),
      employees: Aeson_decode.field("employees", (function (param) {
              return Aeson_decode.list((function (a) {
                            return Aeson_decode.unwrapResult(decodePerson(a));
                          }), param);
            }), json)
    };
  }
  catch (raw_message){
    var message = Caml_js_exceptions.internalToOCamlException(raw_message);
    if (message.RE_EXN_ID === Aeson_decode.DecodeError) {
      return {
              TAG: /* Error */1,
              _0: "decodePerson: " + message._1
            };
    }
    throw message;
  }
  return {
          TAG: /* Ok */0,
          _0: v
        };
}

function encodeShape(x) {
  switch (x.TAG | 0) {
    case /* Square */0 :
        return Aeson_encode.object_({
                    hd: [
                      "tag",
                      "Square"
                    ],
                    tl: {
                      hd: [
                        "contents",
                        [
                          x._0,
                          x._1
                        ]
                      ],
                      tl: /* [] */0
                    }
                  });
    case /* Triangle */1 :
        return Aeson_encode.object_({
                    hd: [
                      "tag",
                      "Triangle"
                    ],
                    tl: {
                      hd: [
                        "contents",
                        [
                          x._0,
                          x._1,
                          x._2
                        ]
                      ],
                      tl: /* [] */0
                    }
                  });
    case /* Rectangle */2 :
        return Aeson_encode.object_({
                    hd: [
                      "tag",
                      "Rectangle"
                    ],
                    tl: {
                      hd: [
                        "contents",
                        [
                          x._0,
                          x._1,
                          x._2,
                          x._3
                        ]
                      ],
                      tl: /* [] */0
                    }
                  });
    
  }
}

function decodeShape(json) {
  var exit = 0;
  var err;
  try {
    err = Aeson_decode.field("tag", Aeson_decode.string, json);
    exit = 1;
  }
  catch (raw_message){
    var message = Caml_js_exceptions.internalToOCamlException(raw_message);
    if (message.RE_EXN_ID === Aeson_decode.DecodeError) {
      return {
              TAG: /* Error */1,
              _0: message._1
            };
    }
    throw message;
  }
  if (exit === 1) {
    switch (err) {
      case "Rectangle" :
          var v = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (v === undefined) {
            return {
                    TAG: /* Error */1,
                    _0: "Rectangle expected an array."
                  };
          }
          var exit$1 = 0;
          var v0;
          try {
            v0 = Aeson_decode.$$int(Caml_array.get(v, 0));
            exit$1 = 2;
          }
          catch (raw_message$1){
            var message$1 = Caml_js_exceptions.internalToOCamlException(raw_message$1);
            if (message$1.RE_EXN_ID === Aeson_decode.DecodeError) {
              return {
                      TAG: /* Error */1,
                      _0: "Rectangle: " + message$1._1
                    };
            }
            throw message$1;
          }
          if (exit$1 === 2) {
            var exit$2 = 0;
            var v1;
            try {
              v1 = Aeson_decode.$$int(Caml_array.get(v, 1));
              exit$2 = 3;
            }
            catch (raw_message$2){
              var message$2 = Caml_js_exceptions.internalToOCamlException(raw_message$2);
              if (message$2.RE_EXN_ID === Aeson_decode.DecodeError) {
                return {
                        TAG: /* Error */1,
                        _0: "Rectangle: " + message$2._1
                      };
              }
              throw message$2;
            }
            if (exit$2 === 3) {
              var exit$3 = 0;
              var v2;
              try {
                v2 = Aeson_decode.$$int(Caml_array.get(v, 2));
                exit$3 = 4;
              }
              catch (raw_message$3){
                var message$3 = Caml_js_exceptions.internalToOCamlException(raw_message$3);
                if (message$3.RE_EXN_ID === Aeson_decode.DecodeError) {
                  return {
                          TAG: /* Error */1,
                          _0: "Rectangle: " + message$3._1
                        };
                }
                throw message$3;
              }
              if (exit$3 === 4) {
                var exit$4 = 0;
                var v3;
                try {
                  v3 = Aeson_decode.$$int(Caml_array.get(v, 3));
                  exit$4 = 5;
                }
                catch (raw_message$4){
                  var message$4 = Caml_js_exceptions.internalToOCamlException(raw_message$4);
                  if (message$4.RE_EXN_ID === Aeson_decode.DecodeError) {
                    return {
                            TAG: /* Error */1,
                            _0: "Rectangle: " + message$4._1
                          };
                  }
                  throw message$4;
                }
                if (exit$4 === 5) {
                  return {
                          TAG: /* Ok */0,
                          _0: {
                            TAG: /* Rectangle */2,
                            _0: v0,
                            _1: v1,
                            _2: v2,
                            _3: v3
                          }
                        };
                }
                
              }
              
            }
            
          }
          break;
      case "Square" :
          var v$1 = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (v$1 === undefined) {
            return {
                    TAG: /* Error */1,
                    _0: "Square expected an array."
                  };
          }
          var exit$5 = 0;
          var v0$1;
          try {
            v0$1 = Aeson_decode.$$int(Caml_array.get(v$1, 0));
            exit$5 = 2;
          }
          catch (raw_message$5){
            var message$5 = Caml_js_exceptions.internalToOCamlException(raw_message$5);
            if (message$5.RE_EXN_ID === Aeson_decode.DecodeError) {
              return {
                      TAG: /* Error */1,
                      _0: "Square: " + message$5._1
                    };
            }
            throw message$5;
          }
          if (exit$5 === 2) {
            var exit$6 = 0;
            var v1$1;
            try {
              v1$1 = Aeson_decode.$$int(Caml_array.get(v$1, 1));
              exit$6 = 3;
            }
            catch (raw_message$6){
              var message$6 = Caml_js_exceptions.internalToOCamlException(raw_message$6);
              if (message$6.RE_EXN_ID === Aeson_decode.DecodeError) {
                return {
                        TAG: /* Error */1,
                        _0: "Square: " + message$6._1
                      };
              }
              throw message$6;
            }
            if (exit$6 === 3) {
              return {
                      TAG: /* Ok */0,
                      _0: {
                        TAG: /* Square */0,
                        _0: v0$1,
                        _1: v1$1
                      }
                    };
            }
            
          }
          break;
      case "Triangle" :
          var v$2 = Aeson_decode.field("contents", Js_json.decodeArray, json);
          if (v$2 === undefined) {
            return {
                    TAG: /* Error */1,
                    _0: "Triangle expected an array."
                  };
          }
          var exit$7 = 0;
          var v0$2;
          try {
            v0$2 = Aeson_decode.$$int(Caml_array.get(v$2, 0));
            exit$7 = 2;
          }
          catch (raw_message$7){
            var message$7 = Caml_js_exceptions.internalToOCamlException(raw_message$7);
            if (message$7.RE_EXN_ID === Aeson_decode.DecodeError) {
              return {
                      TAG: /* Error */1,
                      _0: "Triangle: " + message$7._1
                    };
            }
            throw message$7;
          }
          if (exit$7 === 2) {
            var exit$8 = 0;
            var v1$2;
            try {
              v1$2 = Aeson_decode.$$int(Caml_array.get(v$2, 1));
              exit$8 = 3;
            }
            catch (raw_message$8){
              var message$8 = Caml_js_exceptions.internalToOCamlException(raw_message$8);
              if (message$8.RE_EXN_ID === Aeson_decode.DecodeError) {
                return {
                        TAG: /* Error */1,
                        _0: "Triangle: " + message$8._1
                      };
              }
              throw message$8;
            }
            if (exit$8 === 3) {
              var exit$9 = 0;
              var v2$1;
              try {
                v2$1 = Aeson_decode.$$int(Caml_array.get(v$2, 2));
                exit$9 = 4;
              }
              catch (raw_message$9){
                var message$9 = Caml_js_exceptions.internalToOCamlException(raw_message$9);
                if (message$9.RE_EXN_ID === Aeson_decode.DecodeError) {
                  return {
                          TAG: /* Error */1,
                          _0: "Triangle: " + message$9._1
                        };
                }
                throw message$9;
              }
              if (exit$9 === 4) {
                return {
                        TAG: /* Ok */0,
                        _0: {
                          TAG: /* Triangle */1,
                          _0: v0$2,
                          _1: v1$2,
                          _2: v2$1
                        }
                      };
              }
              
            }
            
          }
          break;
      default:
        return {
                TAG: /* Error */1,
                _0: "Unknown tag value found '" + (err + "'.")
              };
    }
  }
  
}

var Test = {
  encodePerson: encodePerson,
  brokenEncodePerson: brokenEncodePerson,
  decodePerson: decodePerson,
  brokenDecodePerson: brokenDecodePerson,
  encodeCompany: encodeCompany,
  decodeCompany: decodeCompany,
  encodeShape: encodeShape,
  decodeShape: decodeShape
};

AesonSpec.goldenSpec(decodePerson, encodePerson, "person", "__tests__/golden/Person.json");

AesonSpec.goldenDirSpec(decodePerson, encodePerson, "person", "__tests__/golden/Person");

AesonSpec.goldenDirSpec(decodeCompany, encodeCompany, "company", "__tests__/golden/Company");

AesonSpec.goldenDirSpec(decodeShape, encodeShape, "shape", "__tests__/golden/Shape");

AesonSpec_Jest.describe("isJsonFile", (function (param) {
        return AesonSpec_Jest.test("", (function (param) {
                      var files = Fs.readdirSync("__tests__/golden/Person");
                      var filename = Caml_array.get(files, 0);
                      return AesonSpec_Jest.Expect.toEqual(true, AesonSpec_Jest.Expect.expect(AesonSpec.isJsonFile(filename)));
                    }));
      }));

exports.Test = Test;
/*  Not a pure module */
