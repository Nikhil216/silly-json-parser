module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import qualified Parser

main :: IO ()
main =
  do
    args <- getArgs
    let path = head args
    content <- BL.readFile path
    let result = Parser.parse content
    print result
