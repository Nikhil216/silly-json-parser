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


wordToStr :: Word8 -> Char
wordToStr = chr . fromEnum 

matchToken :: Word8 -> [Token] -> [Token]
matchToken word tokens =
  case wordToStr word of
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
    
    '\t' ->
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
              (matchStringToken word BL.empty):tokens
        
        JString str ->
          (matchStringToken word str):rest

        JNumber str ->
          (matchNumberToken word str):rest

        _ ->
          let
            digits = "0123456789"
            isDigit = elem (wordToStr word) digits
          in
            case isDigit of
              True ->
                (matchNumberToken word BL.empty):tokens

              False ->
                JEmpty:tokens

matchStringToken :: Word8 -> BL.ByteString -> Token
matchStringToken word str =
  JString (BL.cons word str)

matchNumberToken :: Word8 -> BL.ByteString -> Token
matchNumberToken word str =
  JNumber (BL.cons word str)

scanner :: BL.ByteString -> [Token]
scanner content =
  (BL.foldr matchToken [] content)
