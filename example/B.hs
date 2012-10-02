
module B where

import C

b :: Int
b = c + 3

data Foo = Bar Int

quux :: Foo
quux = Bar b

flooble :: Int -> Int
flooble foo = foo + (foo * (foo - foo))

