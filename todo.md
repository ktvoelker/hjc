
TODO
====

Implement the native function S to support string literals

  This is probably going to need some reworking, since natives.js doesn't
  know the unique for the list constructor.

Implement enough of IO so that a program can produce output

Support field labels in data constructors

  Remember that the same field label can be used for multiple data constructors
  under a type, even in different positions, but must produce a single binding.

Add lots of native libraries

Add a check somewhere to make sure that Main.main exists and is the right type

