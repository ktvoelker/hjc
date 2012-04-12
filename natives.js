// T is for Type
var T = 42;
// I is for Identity
var I = function(x) { return x; };
// E is for Error
var E = function(msg) {
  throw new Exception(msg);
};
// C is for Constructor
var C = function(con, arity, args) {
  if (arity === 0) {
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
// F is for Force
var F = function(x) {
  if (typeof(x) === 'object' && x.hasOwnProperty('ap')) {
      x.va = F(x.ap)(x.ar);
      delete x.ap;
      delete x.ar;
  }
  return x;
};
// R is for Run
var R = function(io) {
  var cur = io;
  var done = false;
  while (!done) {
    if (cur.hasOwnProperty('io')) {
      cur = cur.fn.call(null);
    } else if (cur.hasOwnProperty('ap')) {
      cur = F(cur);
    } else {
      // error: should be impossible
    }
  }
};
M['GHC.CString'] = {};
M['GHC.CString']['unpackCString#'] = function(str) {
  // TODO
};
