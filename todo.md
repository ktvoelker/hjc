
TODO
====

Implement the native function S to support string literals

  This is probably going to need some reworking, since natives.js doesn't
  know the unique for the list constructor.

Support field labels in data constructors

  Remember that the same field label can be used for multiple data constructors
  under a type, even in different positions, but must produce a single binding.

Start adding native libraries

Add a check somewhere to make sure that Main.main exists and is the right type

