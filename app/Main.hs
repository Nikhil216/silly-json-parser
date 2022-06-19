module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import qualified Scanner

main :: IO ()
main =
  do
    args <- getArgs
    let path = head args
    content <- BL.readFile path
    let result = Scanner.scanner content
    print result
