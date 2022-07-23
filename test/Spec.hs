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

testSuite =
  TestList
    [ testInteger
    , testString
    ]

main :: IO Counts
main = runTestTT testSuite
