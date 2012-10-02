var DEBUG = true;
// T is for Type
var T = 42;
// I is for Identity
var I = function(x) { return x; };
// E is for Error
var E = function(msg) {
  console.error(msg);
  if (DEBUG) {
    debugger;
  } else {
    throw new Exception(msg);
  }
};
// A is for Atom
var A = function() {
  return {co: {}, xs: []};
}
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
  if (typeof(x) === 'object') {
    if (x.hasOwnProperty('ap')) {
      x.va = F(x.ap)(x.ar);
      delete x.ap;
      delete x.ar;
      return x.va;
    } else if (x.hasOwnProperty('va')) {
      return x.va;
    }
  }
  return x;
};
// R is for Run
var R = function(io) {
  F(io).fn.call(null);
};
M['GHC.Base'] = {};
M['GHC.Base']['>>='] = function(t) {
  return function(d) {
    return d['>>='];
  }
};
M['GHC.Base']['return'] = function(t) {
  return function(d) {
    return d['return'];
  }
};
M['GHC.Base']['>>'] = function(t) {
  return function(d) {
    return function(a) {
      return function(b) {
        return M['GHC.Base']['>>='](t)(d)(a)(function(ign) {
          return b;
        });
      };
    };
  };
};
M['GHC.Base']['$fMonadIO'] = {};
M['GHC.Base']['$fMonadIO']['>>='] = function(a) {
  return function(b) {
    return {
      io: true,
      fn: function() {
        return R({ap: b, ar: R(a)});
      }
    };
  };
};
M['GHC.Base']['$fMonadIO']['return'] = function(x) {
  // TODO
};
M['GHC.Base']['++'] = function(t) {
  return function(a) {
    return function(b) {
      // TODO
    };
  };
};
M['GHC.Types'] = {};
M['GHC.Types']['C#'] = I;
M['GHC.Types']['[]'] = function(t) {
  return {co: M['GHC.Types']['[]'], xs: []};
};
M['GHC.Types'][':'] = function(t) {
  return function(head) {
    return function(tail) {
      return {co: M['GHC.Types'][':'], xs: [head, tail]};
    };
  };
};
M['GHC.Tuple'] = {};
M['GHC.Tuple']['()'] = A();
M['GHC.CString'] = {};
M['GHC.CString']['unpackCString#'] = function(str) {
  var ret = {};
  var cur = ret;
  for (var i = 0; i < str.length; ++i) {
    var next = {};
    cur.co = M['GHC.Types'][':'];
    cur.xs = [str[i], next];
    cur = next;
  }
  cur.co = M['GHC.Types']['[]'];
  cur.xs = [];
  return ret;
};
M['System.IO'] = {};
M['System.IO']['putStrLn'] = function(xs) {
  return {io: true, fn: function() {
    var str = '';
    xs = F(xs);
    while (xs.co === M['GHC.Types'][':']) {
      str += xs.xs[0];
      xs = F(xs.xs[1]);
    }
    console.debug(str);
  }};
};
M['GHC.TopHandler'] = {};
M['GHC.TopHandler']['runMainIO'] = function() {
  E("impossible");
};
M[':Main'] = {};
