module Magic where

import System.Environment (getArgs)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as BL


main :: IO ()
main =
  do
    args <- getArgs
    let
      path = head args
    result <- isMagicFile (BL.pack [0x50, 0x4B, 0x03, 0x04]) path
    print result

hasMagic :: BL.ByteString -> BL.ByteString -> Bool
hasMagic magic content =
  BL.take (BL.length magic) content == magic

isMagicFile :: BL.ByteString -> String -> IO Bool
isMagicFile magic path =
  do
    content <- BL.readFile path
    return $ hasMagic magic content

