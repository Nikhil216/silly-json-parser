module Scanner
  ( scan
  ) where

import Data.Char (chr, ord)
import Data.Word (Word8)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Internal as BSI (c2w, w2c)


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

data Match t
  = MatchOk BL.ByteString [t]
  | MatchErr BL.ByteString String
  deriving (Show)

instance Semigroup (Match t) where
  left <> right = 
    case left of
      MatchErr leftStream leftMsg ->
        case right of
          MatchErr rightStream rightMsg ->
            MatchErr rightStream (leftMsg ++ "\n" ++ rightMsg)
          
          MatchOk rightStream rightList ->
            MatchErr rightStream leftMsg

      MatchOk leftStream leftList ->
        case right of
          MatchErr rightStream rightMsg ->
            MatchErr rightStream rightMsg

          MatchOk rightStream rightList ->
            MatchOk rightStream (leftList ++ rightList)

type Scanner t = (BL.ByteString -> Match t)


scan :: BL.ByteString -> ([Token], String)
scan stream =
  let
    matchOpenBrace = matchChar '{'
    matchDoubleQuote = matchChar '\"'
    scanner = matchFoldr matchPipe matchOpenBrace [matchDoubleQuote, matchAlpha, matchAlpha, matchAlpha]
  in
    case scanner stream of 
      MatchOk rest string ->
        ([JString (BLU.fromString string)], "")

      MatchErr rest msg ->
        ([], msg)

matchFoldr :: (Scanner t -> Scanner t -> Scanner t) -> Scanner t -> [Scanner t] -> Scanner t
matchFoldr func init list =
  case list of
    [] ->
      init

    (x:xs) ->
      matchFoldr func (func init x) xs 

matchPipe :: Scanner t -> Scanner t -> Scanner t
matchPipe leftParser rightParser stream =
  let
    leftMatch = leftParser stream
    leftStream =
      case leftMatch of
        MatchOk rest ts ->
          rest

        MatchErr rest msg ->
          rest
    rightMatch = rightParser leftStream
  in
    leftMatch <> rightMatch

matchOr :: Scanner t -> Scanner t -> Scanner t
matchOr leftParser rightParser stream =
  case leftParser stream of
    MatchOk rest ts ->
      MatchOk rest ts

    MatchErr rest msg ->
      case rightParser stream of
        MatchOk rest' ts' ->
          MatchOk rest' ts'

        MatchErr rest' msg' ->
          (MatchErr rest msg) <> (MatchErr rest' msg')

matchMap :: (a -> b) -> Scanner a -> Scanner b
matchMap f scanner =
  (\stream ->
    case scanner stream of
      MatchOk rest ts ->
        MatchOk rest (map f ts)

      MatchErr rest msg ->
        MatchErr rest msg
  )

matchChar :: Char -> BL.ByteString -> Match Char
matchChar char stream =
  case BLU.uncons stream of
    Nothing ->
      MatchErr BL.empty "End of stream"

    Just (c, rest) ->
      MatchOk rest [c]

matchAlphaLower :: BL.ByteString -> Match Char
matchAlphaLower =
  matchFoldr matchOr (matchChar 'a') (map matchChar "bcdefghijlkmnopqrstvwxyz")

matchAlphaUpper :: BL.ByteString -> Match Char
matchAlphaUpper =
  matchFoldr matchOr (matchChar 'A') (map matchChar "BCDEFGHIJKLMNOPQRSTUVWXYZ")

matchDigits :: BL.ByteString -> Match Char
matchDigits =
  matchFoldr matchOr (matchChar '0') (map matchChar "123456789")

matchAlpha :: BL.ByteString -> Match Char
matchAlpha =
  matchAlphaLower `matchOr` matchAlphaUpper
