// Generated by BUCKLESCRIPT VERSION 4.0.11, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Js_json = require("bs-platform/lib/js/js_json.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Api$ReasonDojoPokedex = require("./Api.bs.js");
var Link$ReasonDojoPokedex = require("./Link.bs.js");

var ppx_printed_query = "query getPokemon($identifier: String)  {\npokemon: Pokemon(filter: {identifier: $identifier})  {\nedges  {\nnode  {\nid  \nenglishName  \nidentifier  \n}\n\n}\n\n}\n\n}\n";

function parse(value) {
  var match = Js_json.decodeObject(value);
  if (match !== undefined) {
    var match$1 = Js_dict.get(Caml_option.valFromOption(match), "pokemon");
    var tmp;
    if (match$1 !== undefined) {
      var value$1 = Caml_option.valFromOption(match$1);
      var match$2 = Js_json.decodeNull(value$1);
      if (match$2 !== undefined) {
        tmp = undefined;
      } else {
        var match$3 = Js_json.decodeObject(value$1);
        var tmp$1;
        if (match$3 !== undefined) {
          var match$4 = Js_dict.get(Caml_option.valFromOption(match$3), "edges");
          var tmp$2;
          if (match$4 !== undefined) {
            var value$2 = Caml_option.valFromOption(match$4);
            var match$5 = Js_json.decodeNull(value$2);
            if (match$5 !== undefined) {
              tmp$2 = undefined;
            } else {
              var match$6 = Js_json.decodeArray(value$2);
              tmp$2 = match$6 !== undefined ? match$6.map((function (value) {
                        var match = Js_json.decodeNull(value);
                        if (match !== undefined) {
                          return undefined;
                        } else {
                          var match$1 = Js_json.decodeObject(value);
                          var tmp;
                          if (match$1 !== undefined) {
                            var match$2 = Js_dict.get(Caml_option.valFromOption(match$1), "node");
                            var tmp$1;
                            if (match$2 !== undefined) {
                              var value$1 = Caml_option.valFromOption(match$2);
                              var match$3 = Js_json.decodeNull(value$1);
                              if (match$3 !== undefined) {
                                tmp$1 = undefined;
                              } else {
                                var match$4 = Js_json.decodeObject(value$1);
                                var tmp$2;
                                if (match$4 !== undefined) {
                                  var value$2 = Caml_option.valFromOption(match$4);
                                  var match$5 = Js_dict.get(value$2, "id");
                                  var tmp$3;
                                  if (match$5 !== undefined) {
                                    var value$3 = Caml_option.valFromOption(match$5);
                                    var match$6 = Js_json.decodeNull(value$3);
                                    if (match$6 !== undefined) {
                                      tmp$3 = undefined;
                                    } else {
                                      var match$7 = Js_json.decodeString(value$3);
                                      tmp$3 = match$7 !== undefined ? match$7 : Js_exn.raiseError("graphql_ppx: Expected string, got " + JSON.stringify(value$3));
                                    }
                                  } else {
                                    tmp$3 = undefined;
                                  }
                                  var match$8 = Js_dict.get(value$2, "englishName");
                                  var tmp$4;
                                  if (match$8 !== undefined) {
                                    var value$4 = Caml_option.valFromOption(match$8);
                                    var match$9 = Js_json.decodeNull(value$4);
                                    if (match$9 !== undefined) {
                                      tmp$4 = undefined;
                                    } else {
                                      var match$10 = Js_json.decodeString(value$4);
                                      tmp$4 = match$10 !== undefined ? match$10 : Js_exn.raiseError("graphql_ppx: Expected string, got " + JSON.stringify(value$4));
                                    }
                                  } else {
                                    tmp$4 = undefined;
                                  }
                                  var match$11 = Js_dict.get(value$2, "identifier");
                                  var tmp$5;
                                  if (match$11 !== undefined) {
                                    var value$5 = Caml_option.valFromOption(match$11);
                                    var match$12 = Js_json.decodeNull(value$5);
                                    if (match$12 !== undefined) {
                                      tmp$5 = undefined;
                                    } else {
                                      var match$13 = Js_json.decodeString(value$5);
                                      tmp$5 = match$13 !== undefined ? match$13 : Js_exn.raiseError("graphql_ppx: Expected string, got " + JSON.stringify(value$5));
                                    }
                                  } else {
                                    tmp$5 = undefined;
                                  }
                                  tmp$2 = {
                                    id: tmp$3,
                                    englishName: tmp$4,
                                    identifier: tmp$5
                                  };
                                } else {
                                  tmp$2 = Js_exn.raiseError("graphql_ppx: Object is not a value");
                                }
                                tmp$1 = Caml_option.some(tmp$2);
                              }
                            } else {
                              tmp$1 = undefined;
                            }
                            tmp = {
                              node: tmp$1
                            };
                          } else {
                            tmp = Js_exn.raiseError("graphql_ppx: Object is not a value");
                          }
                          return Caml_option.some(tmp);
                        }
                      })) : Js_exn.raiseError("graphql_ppx: Expected array, got " + JSON.stringify(value$2));
            }
          } else {
            tmp$2 = undefined;
          }
          tmp$1 = {
            edges: tmp$2
          };
        } else {
          tmp$1 = Js_exn.raiseError("graphql_ppx: Object is not a value");
        }
        tmp = Caml_option.some(tmp$1);
      }
    } else {
      tmp = undefined;
    }
    return {
            pokemon: tmp
          };
  } else {
    return Js_exn.raiseError("graphql_ppx: Object is not a value");
  }
}

function make(identifier, param) {
  return {
          query: ppx_printed_query,
          variables: Js_dict.fromArray(/* array */[/* tuple */[
                  "identifier",
                  identifier !== undefined ? identifier : null
                ]]),
          parse: parse
        };
}

function makeWithVariables(variables) {
  var identifier = variables.identifier;
  return {
          query: ppx_printed_query,
          variables: Js_dict.fromArray(/* array */[/* tuple */[
                  "identifier",
                  identifier !== undefined ? identifier : null
                ]]),
          parse: parse
        };
}

function ret_type(f) {
  return /* module */[];
}

var MT_Ret = /* module */[];

var Query = /* module */[
  /* ppx_printed_query */ppx_printed_query,
  /* query */ppx_printed_query,
  /* parse */parse,
  /* make */make,
  /* makeWithVariables */makeWithVariables,
  /* ret_type */ret_type,
  /* MT_Ret */MT_Ret
];

function decode(response) {
  var match = response.pokemon;
  var edges = match !== undefined ? Caml_option.valFromOption(match).edges : undefined;
  var pokemons = edges !== undefined ? Belt_List.map(Belt_List.keep(Belt_List.fromArray(Belt_Array.map(edges, (function (edge) {
                        var match = edge !== undefined ? Caml_option.valFromOption(edge).node : undefined;
                        var identifier = match !== undefined ? Caml_option.valFromOption(match).identifier : undefined;
                        var match$1 = edge !== undefined ? Caml_option.valFromOption(edge).node : undefined;
                        var id = match$1 !== undefined ? Caml_option.valFromOption(match$1).id : undefined;
                        var match$2 = edge !== undefined ? Caml_option.valFromOption(edge).node : undefined;
                        var englishName = match$2 !== undefined ? Caml_option.valFromOption(match$2).englishName : undefined;
                        return Belt_Option.flatMap(identifier, (function (identifier) {
                                      return Belt_Option.flatMap(id, (function (id) {
                                                    return Belt_Option.flatMap(englishName, (function (englishName) {
                                                                  return /* record */[
                                                                          /* identifier */identifier,
                                                                          /* id */Caml_format.caml_int_of_string(id),
                                                                          /* englishName */englishName
                                                                        ];
                                                                }));
                                                  }));
                                    }));
                      }))), Belt_Option.isSome), Belt_Option.getExn) : /* [] */0;
  if (pokemons && !pokemons[1]) {
    return /* Ok */Block.__(0, [pokemons[0]]);
  } else {
    return /* Error */Block.__(1, ["bad data"]);
  }
}

var component = ReasonReact.reducerComponent("Pokemon");

function make$1(identifier, _children) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              Api$ReasonDojoPokedex.sendQuery(make(identifier, /* () */0)).then((function (r) {
                      if (r.tag) {
                        Curry._1(self[/* send */3], /* SetError */Block.__(1, [r[0]]));
                      } else {
                        var match = decode(r[0]);
                        if (match.tag) {
                          Curry._1(self[/* send */3], /* SetError */Block.__(1, [match[0]]));
                        } else {
                          Curry._1(self[/* send */3], /* LoadData */Block.__(0, [match[0]]));
                        }
                      }
                      return Promise.resolve(/* () */0);
                    }));
              return /* () */0;
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              var match = self[/* state */1];
              if (typeof match === "number") {
                return React.createElement("div", undefined, "Loading...");
              } else if (match.tag) {
                return React.createElement("div", undefined, match[0][/* englishName */2], React.createElement("div", undefined, ReasonReact.element(undefined, undefined, Link$ReasonDojoPokedex.make("/", "Back"))));
              } else {
                return React.createElement("div", undefined, match[0]);
              }
            }),
          /* initialState */(function (param) {
              return /* Loading */0;
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, _state) {
              if (action.tag) {
                return /* Update */Block.__(0, [/* Error */Block.__(0, [action[0]])]);
              } else {
                return /* Update */Block.__(0, [/* Data */Block.__(1, [action[0]])]);
              }
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.Query = Query;
exports.decode = decode;
exports.component = component;
exports.make = make$1;
/* component Not a pure module */