var T = 42;
var I = function(x) { return x; }
var E = function(msg) {
  throw new Exception(msg);
};
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
var S = function(str) {
  // TODO: convert str to an algebraic string
};
var F = function(x) {
  if (typeof(x) === 'object' && x.hasOwnProperty('ap')) {
      x.va = F(x.ap)(x.ar);
      delete x.ap;
      delete x.ar;
  } else {
    return x;
  }
};
