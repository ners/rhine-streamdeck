module Main where

import MyLib qualified (someFunc)
import Prelude

main :: IO ()
main = MyLib.someFunc
