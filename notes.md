
From ECMA-262, page 29:
"Note that all the positive and negative integers whose magnitude is no greater
than 2^53 are representable in the Number type (indeed, the integer 0 has two
representations, +0 and -0)."

From the Haskell 98 Report, section 6.4:
"The finite-precision integer type Int covers at least the range [-2^29,
2^29-1]."


Haskell value        JavaScript value
=============        ================
Unforced application {ap: Function, ar: VALUE}
Forced application   {va: VALUE}
Int                  Number
Double               Number
Algebraic value      {co: CTOR, xs: [VALUE, ...]}
Function             Function
Type                 null
IO monad value       {io: true, fn: Function}
IO monad done        {io: false}

VALUE is any value.

CTOR is a special object reference which provides the identity of the
constructor.

JavaScript Functions corresponding to Haskell functions always accept exactly one
argument and return a VALUE.

"Native" functions do not have a special representation: they are merely
custom-implemented JavaScript functions that behave the same way as JavaScript
functions produced through execution of compiled Haskell.

