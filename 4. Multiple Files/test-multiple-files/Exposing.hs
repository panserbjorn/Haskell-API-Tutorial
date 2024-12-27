module Exposing
  ( myFunction1,
    myFunction2,
    MyType1,
  )
where

myFunction1 :: Int -> Int
myFunction1 x = x + 1

myFunction2 :: Int -> Int
myFunction2 x = myFunction3 x - 1

myFunction3 :: Int -> Int
myFunction3 x = x + 3

data MyType1 = MyInt1 Int | MyString1 String

data MyType2 = MyInt2 Int | MyString2 String
