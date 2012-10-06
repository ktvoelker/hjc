
module B where

import C

b :: Int
b = c + 3

data Foo = Bar { fooBar :: Int }

quux :: Foo
quux = Bar b

flooble :: Int -> Int
flooble foo = foo + (foo * (foo - foo))

erpt :: Int
erpt = fooBar quux

