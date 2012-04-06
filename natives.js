// T is for Type
var T = 42;
// I is for Identity
var I = function(x) { return x; }
// E is for Error
var E = function(msg) {
  throw new Exception(msg);
};
// C is for Constructor
var C = function(con, arity, args) {
  if (arity == 0) {
    return {co: con, xs: args};
  } else {
    return function(arg) {
      var moreArgs = args.forEach(I);
      moreArgs.push(arg);
      return C(con, arity, moreArgs);
    };
  }
};
// S is for String
var S = function(str) {
  // TODO: convert str to an algebraic string
};
// F is for Force
var F = function(x) {
  if (typeof(x) === 'object' && x.hasOwnProperty('ap')) {
      x.va = F(x.ap)(x.ar);
      delete x.ap;
      delete x.ar;
  } else {
    return x;
  }
};
// R is for Run
var R = function(io) {
  var cur = io;
  while (cur.io) {
    cur = cur.fn.call(null);
  }
};

