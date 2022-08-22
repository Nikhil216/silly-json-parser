import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Scanner as S
import Test.HUnit


testNot =
  let
    matchNotDigit =  S.matchNot id S.matchDigit
  in
    TestList
      [ TestCase
          (assertEqual
            "Should not match any digit"
            (S.MatchErr (BLU.fromString "123") "Not expected but found it")
            (matchNotDigit . BLU.fromString $ "123"))
      , TestCase
          (assertEqual
            "Should match spaces"
            (S.MatchOk BL.empty " ")
            (matchNotDigit . BLU.fromString $ " "))
      ]

testInteger =
  TestList
    [ TestCase
        (assertEqual
          "Empty String should be matched as end of stream"
          (S.MatchEnd "")
          (S.matchInteger . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should not match alphabet"
          (S.MatchErr (BLU.fromString "abc") "Found a instead of 9")
          (S.matchInteger . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "Should match single digit"
          (S.MatchOk BL.empty "1")
          (S.matchInteger . BLU.fromString $ "1"))
    , TestCase
        (assertEqual
          "Should match multiple digits"
          (S.MatchOk BL.empty "31416")
          (S.matchInteger . BLU.fromString $ "31416"))
    , TestCase
        (assertEqual
          "Should match multiple digits and not the rest"
          (S.MatchOk (BLU.fromString ".9") "31415")
          (S.matchInteger . BLU.fromString $ "31415.9"))
    ]

testString =
  TestList
    [ TestCase
        (assertEqual
          "Empty String should be matched as end of stream"
          (S.MatchEnd "")
          (S.matchString . BLU.fromString $ ""))
    -- , TestCase
    --     (assertEqual
    --       "Should not match alphabet"
    --       (S.MatchErr (BLU.fromString "abc") "Found a instead of \"")
    --       (S.matchString . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "Should match empty string"
          (S.MatchOk BL.empty "\"\"")
          (S.matchString . BLU.fromString $ "\"\""))
    , TestCase
        (assertEqual
          "Should match string"
          (S.MatchOk BL.empty "\"hello\"")
          (S.matchString . BLU.fromString $ "\"hello\""))
    , TestCase
        (assertEqual
          "Should match string and not the rest"
          (S.MatchOk (BLU.fromString "world") "\"hello\"")
          (S.matchString . BLU.fromString $ "\"hello\"world"))
    ]

testList =
  TestList
    [ TestCase
        (assertEqual
          "Should escape on end stream"
          (S.MatchEnd "")
          (S.matchList . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should match empty array"
          (S.MatchOk BL.empty "[]")
          (S.matchList . BLU.fromString $ "[]"))
    , TestCase
        (assertEqual
          "Should match single element"
          (S.MatchOk BL.empty "[1]")
          (S.matchList . BLU.fromString $ "[1]"))
    ]

testNotChar =
  TestList
    [ TestCase
        (assertEqual
          "Should escape on end stream"
          (S.MatchEnd "")
          ((S.matchNotChar '"') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should error on double quote"
          (S.MatchErr (BLU.fromString "\"") "Not expected but found it")
          ((S.matchNotChar '"') . BLU.fromString $ "\""))
    , TestCase
        (assertEqual
          "Should match on space"
          (S.MatchOk BL.empty " ")
          ((S.matchNotChar '"') . BLU.fromString $ " "))
    ]

testPeek =
  TestList
    [ TestCase
        (assertEqual
          "Should escape on end stream"
          (S.MatchEnd "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should error on peeking 'a'"
          (S.MatchErr (BLU.fromString "a ") "Found a instead of  ")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "Should match on single ' ' without trail"
          (S.MatchOk (BLU.fromString " ") "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "Should match on single ' ' with trail"
          (S.MatchOk (BLU.fromString " a") "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ " a"))
    , TestCase
        (assertEqual
          "Should match on single '  ' with ' ' as trail"
          (S.MatchOk (BLU.fromString "  ") "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ "  "))
    ]

testPeekNot =
  TestList
    [ TestCase
        (assertEqual
          "Should escape on end stream"
          (S.MatchEnd "")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should match on peeking 'a'"
          (S.MatchOk (BLU.fromString "a ") "")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "Should error on single ' ' without trail"
          (S.MatchErr (BLU.fromString " ") "Not expected but found it")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "Should error on double ' ' without trail"
          (S.MatchErr (BLU.fromString "  ") "Not expected but found it")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ "  "))
    , TestCase
        (assertEqual
          "Should match on double 'b' without trail"
          (S.MatchOk (BLU.fromString "bb") "")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ "bb"))
    ]

testOr =
  TestList
    [ TestCase
        (assertEqual
          "Should escape on end stream"
          (S.MatchEnd "")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should match on 'a'"
          (S.MatchOk (BLU.fromString "bc") "a")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "Should match on 'b'"
          (S.MatchOk (BLU.fromString "ba") "b")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ "bba"))
    , TestCase
        (assertEqual
          "Should error on 'c'"
          (S.MatchErr (BLU.fromString "ccc") "Found c instead of b")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ "ccc"))
    ]

testZeroOrMoreChar =
  TestList
    [ TestCase
        (assertEqual
          "Should escape on end stream"
          (S.MatchEnd "")
          ((S.matchZeroOrMoreChar $ S.matchChar '"') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "Should match on single 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "a")
          ((S.matchZeroOrMoreChar $ S.matchChar '"') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "Should match on single 'a' without trail"
          (S.MatchOk BL.empty "a")
          ((S.matchZeroOrMoreChar $ S.matchChar '"') . BLU.fromString $ "a"))
    , TestCase
        (assertEqual
          "Should match on triple 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "aaa")
          ((S.matchZeroOrMoreChar $ S.matchChar '"') . BLU.fromString $ "aaa "))
    , TestCase
        (assertEqual
          "Should match on triple 'a' without trail"
          (S.MatchOk BL.empty "aaa")
          ((S.matchZeroOrMoreChar $ S.matchChar '"') . BLU.fromString $ "aaa"))
    ]

testSuite =
  TestList
    [ testNotChar
    , testPeek
    , testOr
    , testPeekNot
    , testZeroOrMoreChar
    , testInteger
    , testString
    , testList
    ]

main :: IO Counts
main = runTestTT testSuite
