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
    scanner = matchData
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

matchNot :: (Char -> t) -> Scanner t -> Scanner t
matchNot f scanner stream =
  case scanner stream of
    MatchOk rest a ->
      MatchErr stream ("Not expected but found it")

    MatchErr rest msg ->
      if msg == "End of stream"
        then
          MatchErr rest msg
        else
          matchAny f stream

matchZeroOrMore :: (Char -> t) -> Scanner t -> Scanner t
matchZeroOrMore f scanner =
  (peek (matchNot f scanner)) `matchOr` (scanner `matchPipe` (matchZeroOrMore f scanner))

matchZeroOrMoreChar :: Scanner Char -> Scanner Char
matchZeroOrMoreChar = matchZeroOrMore id

matchOneOrMore :: (Char -> t) -> Scanner t -> Scanner t
matchOneOrMore f scanner =
  scanner `matchPipe` (matchZeroOrMore f scanner)

matchOneOrMoreChar :: Scanner Char -> Scanner Char
matchOneOrMoreChar = matchOneOrMore id

matchMap :: (a -> b) -> Scanner a -> Scanner b
matchMap f scanner stream =
  case scanner stream of
    MatchOk rest ts ->
      MatchOk rest (map f ts)

    MatchErr rest msg ->
      MatchErr rest msg

matchAny :: (Char -> t) -> Scanner t
matchAny f stream =
  case BLU.uncons stream of
    Nothing ->
      MatchErr BL.empty "End of stream"

    Just (c, rest) ->
      MatchOk rest [f c]

matchAnyChar :: Scanner Char
matchAnyChar =
  matchAny id

matchChar :: Char -> Scanner Char
matchChar char stream =
  case BLU.uncons stream of
    Nothing ->
      MatchErr BL.empty "End of stream"

    Just (c, rest) ->
      if c == char
        then
          MatchOk rest [c]
        else
          MatchErr stream ("Found " ++ [c] ++ " instead of " ++ [char])

peek :: Scanner t -> Scanner t
peek scanner stream =
  case scanner stream of
    MatchErr rest msg ->
      MatchErr rest msg

    MatchOk rest ts ->
      MatchOk stream []

matchNotChar :: Char -> Scanner Char
matchNotChar = matchNot id . matchChar

matchAlphaLower :: Scanner Char
matchAlphaLower =
  matchFoldr matchOr (matchChar 'a') (map matchChar "bcdefghijlkmnopqrstvwxyz")

matchAlphaUpper :: Scanner Char
matchAlphaUpper =
  matchFoldr matchOr (matchChar 'A') (map matchChar "BCDEFGHIJKLMNOPQRSTUVWXYZ")

matchDigit :: Scanner Char
matchDigit =
  matchFoldr matchOr (matchChar '0') (map matchChar "123456789")

matchAlpha :: Scanner Char
matchAlpha =
  matchAlphaLower `matchOr` matchAlphaUpper

matchInteger :: Scanner Char
matchInteger =
  matchOneOrMoreChar matchDigit

matchFloat :: Scanner Char
matchFloat =
  matchInteger `matchPipe` (matchChar '.') `matchPipe` matchInteger

matchNumber :: Scanner Char
matchNumber =
  matchFloat `matchOr` matchInteger

matchWhiteSpace :: Scanner Char
matchWhiteSpace =
  matchFoldr matchOr (matchChar ' ') (map matchChar "\t\r\n")

matchMultipleWhiteSpace :: Scanner Char
matchMultipleWhiteSpace = matchZeroOrMoreChar matchWhiteSpace

matchString :: Scanner Char
matchString =
  (matchChar '"') `matchPipe` (matchZeroOrMoreChar (matchNotChar '"')) `matchPipe` (matchChar '"')

matchCollection :: Scanner Char -> Scanner Char -> Scanner Char -> Scanner Char
matchCollection matchOpen matchClose matchItem =
  let
    matchComma = matchChar ','
    matchCollectionEnd =
      matchOr
        matchClose
        (matchFoldr
          matchPipe
          matchComma
          [ matchMultipleWhiteSpace
          , matchItem
          , matchMultipleWhiteSpace
          , matchCollectionEnd
          ])
  in
    matchFoldr
      matchPipe
      matchOpen
      [ matchMultipleWhiteSpace
      , matchClose `matchOr` (matchItem `matchPipe` matchMultipleWhiteSpace `matchPipe` matchCollectionEnd)
      ]

matchList :: Scanner Char
matchList =
  let
    matchOpenSq = matchChar '['
    matchCloseSq = matchChar ']'
  in
    matchCollection matchOpenSq matchCloseSq matchData

matchData :: Scanner Char
matchData =
  matchFoldr
    matchOr
    matchString
    [ matchNumber
    , matchList
    , matchObject
    ]

matchKeyValuePair :: Scanner Char
matchKeyValuePair =
  let
    matchColon = matchChar ':'
  in
    matchFoldr
      matchPipe
      matchString
      [ matchMultipleWhiteSpace
      , matchColon
      , matchMultipleWhiteSpace
      , matchData
      ]

matchObject :: Scanner Char
matchObject =
  let
    matchOpenBrace = matchChar '{'
    matchCloseBrace = matchChar '}'
  in
    matchCollection matchOpenBrace matchCloseBrace matchKeyValuePair
