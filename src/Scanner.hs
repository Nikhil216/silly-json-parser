module Scanner
  ( scanner
  ) where

import Data.Char (chr)
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU


data Token
  = JOpeningBrace
  | JClosingBrace
  | JOpeningSquare
  | JClosingSquare
  | JComma
  | JColon
  | JDoubleQuote
  | JTrue
  | JFalse
  | JNull
  | JString BL.ByteString
  | JNumber BL.ByteString
  | JWhiteSpace
  | JEmpty
  deriving (Show)

matchToken :: Word8 -> [Token] -> [Token]
matchToken word tokens =
  case (chr . fromEnum) word of
    '{' ->
      JOpeningBrace:tokens
    
    '}' ->
      JClosingBrace:tokens
    
    '[' ->
      JOpeningSquare:tokens
    
    ']' ->
      JClosingSquare:tokens
    
    ',' ->
      JComma:tokens
    
    ':' ->
      JColon:tokens
    
    '"' ->
      JDoubleQuote:tokens
    
    ' ' ->
      JWhiteSpace:tokens

    '\n' ->
      JWhiteSpace:tokens
    
    _ ->
      matchElse word tokens


matchElse :: Word8 -> [Token] -> [Token]
matchElse word tokens =
  case tokens of
    [] ->
      matchToken word []
    
    (current:rest) ->
      case current of
        JDoubleQuote ->
          case rest of
            (JString _:rest') ->
              matchToken word tokens
            
            _ ->
              JString (BL.cons word BL.empty):tokens
        
        JString str ->
          JString (BL.cons word str):rest

        _ ->
          JEmpty:tokens


scanner :: BL.ByteString -> [Token]
scanner content =
  (BL.foldr matchToken [] content)
