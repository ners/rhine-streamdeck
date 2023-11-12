module Main where

import MyLib qualified (someFunc)
import Prelude

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    MyLib.someFunc
