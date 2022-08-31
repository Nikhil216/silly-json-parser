module Scanner
  ( Token (..)
  , Match (..)
  , Scanner
  , scan
  , matchFoldr
  , matchPipe
  , matchOr
  , matchNot
  , matchZeroOrMore
  , matchZeroOrMoreChar
  , matchOneOrMore
  , matchOneOrMoreChar
  , matchZeroOrOne
  , matchZeroOrOneChar
  , matchMap
  , matchAny
  , matchAnyChar
  , matchChar
  , peek
  , matchNotChar
  , matchAlphaLower
  , matchAlphaUpper
  , matchDigit
  , matchAlpha
  , matchInteger
  , matchFloat
  , matchNumber
  , matchWhiteSpace
  , matchMultipleWhiteSpace
  , matchString
  , matchCollection
  , matchList
  , matchData
  , matchKeyValuePair
  , matchObject
  ) where

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
  deriving (Show, Eq)

data Match t
  = MatchOk BL.ByteString [t]
  | MatchErr BL.ByteString String
  | MatchEnd [t]
  deriving (Show, Eq)

instance Semigroup (Match t) where
  left <> right = 
    case left of
      MatchEnd ts ->
        MatchEnd ts

      MatchErr leftStream leftMsg ->
        MatchErr leftStream leftMsg

      MatchOk leftStream leftList ->
        case right of
          MatchEnd ts ->
            MatchEnd (leftList ++ ts)
          
          MatchErr rightStream rightMsg ->
            MatchErr rightStream rightMsg

          MatchOk rightStream rightList ->
            MatchOk rightStream (leftList ++ rightList)

type Scanner t = (BL.ByteString -> Match t)


scan :: BL.ByteString -> ([Token], String)
scan stream =
  let
    scanner = matchObject
  in
    case scanner stream of
      MatchEnd ts ->
        ([JString (BLU.fromString ts)], "End of Stream")
      
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
        MatchEnd ts ->
          BL.empty
        
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
    MatchEnd ts ->
      MatchEnd ts

    MatchErr rest msg ->
      case rightParser stream of
        MatchEnd ts ->
          MatchEnd ts
        
        MatchOk rest' ts' ->
          MatchOk rest' ts'

        MatchErr rest' msg' ->
          (MatchErr rest' msg')

    MatchOk rest ts ->
      MatchOk rest ts

matchNot :: (Char -> t) -> Scanner t -> Scanner t
matchNot f scanner stream =
  case scanner stream of
    MatchEnd ts ->
      MatchEnd ts
    
    MatchOk rest a ->
      MatchErr stream "Not expected but found it"

    MatchErr rest msg ->
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

matchZeroOrOne :: (Char -> t) -> Scanner t -> Scanner t
matchZeroOrOne f scanner =
  scanner `matchOr` (peek (matchAny f))

matchZeroOrOneChar :: Scanner Char -> Scanner Char
matchZeroOrOneChar = matchZeroOrOne id


matchMap :: (a -> b) -> Scanner a -> Scanner b
matchMap f scanner stream =
  case scanner stream of
    MatchEnd ts ->
      MatchEnd (map f ts)
    
    MatchOk rest ts ->
      MatchOk rest (map f ts)

    MatchErr rest msg ->
      MatchErr rest msg

matchAny :: (Char -> t) -> Scanner t
matchAny f stream =
  case BLU.uncons stream of
    Nothing ->
      MatchEnd []

    Just (c, rest) ->
      MatchOk rest [f c]

matchAnyChar :: Scanner Char
matchAnyChar =
  matchAny id

matchChar :: Char -> Scanner Char
matchChar char stream =
  case BLU.uncons stream of
    Nothing ->
      MatchEnd []

    Just (c, rest) ->
      if c == char
        then
          MatchOk rest [c]
        else
          MatchErr stream ("Found " ++ [c] ++ " instead of " ++ [char])

peek :: Scanner t -> Scanner t
peek scanner stream =
  case scanner stream of
    MatchEnd ts ->
      MatchEnd ts
    
    MatchErr rest msg ->
      MatchErr stream msg

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
    matchEmptyOrEnd =
      matchOr
        matchClose
        (matchFoldr
          matchPipe
          matchItem
          [ matchMultipleWhiteSpace
          , matchCollectionEnd
          ])
  in
    matchFoldr
      matchPipe
      matchOpen
      [ matchMultipleWhiteSpace
      , matchEmptyOrEnd
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
    matchNumber
    [ matchString
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

{-
alphaLower <- "a" .. "z"
alphaUpper <- "A" .. "Z"
digits <- "0" .. "9"
integer <- digits+
float <- integer >> "." >> integer
number <- float | integer
whiteSpace <- " " | "\t" | "\r" | "\n"
mws <- whiteSpace*
string <- """ >> (~""")* >> """
listEnd <- "]" | ("," >> mws >> number >> mws >> listEnd)
list <- "[" >> mws >> ("]" | (number >> mws >> listEnd))
pair <- string >> mws >> ":" >> mws >> list
objEnd <- "}" | ("," >> mws >> pair >> mws >> objEnd)
obj <- "{" >> mws >> ("}" | (pair >> mws >> objEnd))
-}
