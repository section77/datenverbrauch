module Main where

import           BasicPrelude
import           Test.DocTest

main :: IO ()
main = doctest [
    "-packageghc"
  , "-isrc"
  , "src"
  ]
