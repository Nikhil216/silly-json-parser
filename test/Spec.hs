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
            "testNot - Should not match any digit"
            (S.MatchErr (BLU.fromString "123") "Not expected but found it")
            (matchNotDigit . BLU.fromString $ "123"))
      , TestCase
          (assertEqual
            "testNot - Should match spaces"
            (S.MatchOk BL.empty " ")
            (matchNotDigit . BLU.fromString $ " "))
      ]

testInteger =
  TestList
    [ TestCase
        (assertEqual
          "testInteger - Empty String should be matched as end of stream"
          (S.MatchEnd "")
          (S.matchInteger . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testInteger - Should not match alphabet"
          (S.MatchErr (BLU.fromString "abc") "Found a instead of 9")
          (S.matchInteger . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "testInteger - Should not match on single -"
          (S.MatchErr (BLU.fromString "-") "Found - instead of 9")
          (S.matchInteger . BLU.fromString $ "--"))
    , TestCase
        (assertEqual
          "testInteger - Should match single digit"
          (S.MatchEnd "1")
          (S.matchInteger . BLU.fromString $ "1"))
    , TestCase
        (assertEqual
          "testInteger - Should match multiple digits"
          (S.MatchEnd "31416")
          (S.matchInteger . BLU.fromString $ "31416"))
    , TestCase
        (assertEqual
          "testInteger - Should match multiple digits and not the rest"
          (S.MatchOk (BLU.fromString ".9") "31415")
          (S.matchInteger . BLU.fromString $ "31415.9"))
    , TestCase
        (assertEqual
          "testInteger - Should match negative number"
          (S.MatchEnd "-31415")
          (S.matchInteger . BLU.fromString $ "-31415"))
    ]

testFloat =
  TestList
    [ TestCase
        (assertEqual
          "testFloat - Empty String should be matched as end of stream"
          (S.MatchEnd "")
          (S.matchFloat . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testFloat - Should not match alphabet"
          (S.MatchErr (BLU.fromString "abc") "Found a instead of 9")
          (S.matchFloat . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "testFloat - Should not match on single -"
          (S.MatchErr (BLU.fromString "-") "Found - instead of 9")
          (S.matchFloat . BLU.fromString $ "--"))
    , TestCase
        (assertEqual
          "testFloat - Should match single digits on each side of decimal point"
          (S.MatchEnd "1.0")
          (S.matchFloat . BLU.fromString $ "1.0"))
    , TestCase
        (assertEqual
          "testFloat - Should match multiple zeros"
          (S.MatchEnd "00000")
          (S.matchFloat . BLU.fromString $ "00000"))
    , TestCase
        (assertEqual
          "testFloat - Should match mulitple digits on each side of decimal point"
          (S.MatchEnd "420.69")
          (S.matchFloat . BLU.fromString $ "420.69"))
    , TestCase
        (assertEqual
          "testFloat - Should match pi"
          (S.MatchEnd "3.1416")
          (S.matchFloat . BLU.fromString $ "3.1416"))
    , TestCase
        (assertEqual
          "testFloat - Should match e"
          (S.MatchEnd "2.71828182846")
          (S.matchFloat . BLU.fromString $ "2.71828182846"))
    , TestCase
        (assertEqual
          "testFloat - Should match negative pi"
          (S.MatchEnd "-3.1415")
          (S.matchFloat . BLU.fromString $ "-3.1415"))
    ]

testString =
  TestList
    [ TestCase
        (assertEqual
          "testString - Empty String should be matched as end of stream"
          (S.MatchEnd "")
          (S.matchString . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testString - Should not match alphabet"
          (S.MatchErr (BLU.fromString "abc") "Found a instead of \"")
          (S.matchString . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "testString - Should match empty string"
          (S.MatchOk BL.empty "\"\"")
          (S.matchString . BLU.fromString $ "\"\""))
    , TestCase
        (assertEqual
          "testString - Should match string"
          (S.MatchOk BL.empty "\"hello\"")
          (S.matchString . BLU.fromString $ "\"hello\""))
    , TestCase
        (assertEqual
          "testString - Should match string and not the rest"
          (S.MatchOk (BLU.fromString "world") "\"hello\"")
          (S.matchString . BLU.fromString $ "\"hello\"world"))
    ]

testList =
  TestList
    [ TestCase
        (assertEqual
          "testList - Should escape on end stream"
          (S.MatchEnd "")
          (S.matchList . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testList - Should error out on space"
          (S.MatchErr (BLU.fromString " ") "Found   instead of [")
          (S.matchList . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "testList - Should match empty array"
          (S.MatchOk BL.empty "[]")
          (S.matchList . BLU.fromString $ "[]"))
    , TestCase
        (assertEqual
          "testList - Should match single number"
          (S.MatchOk BL.empty "[1]")
          (S.matchList . BLU.fromString $ "[1]"))
    , TestCase
        (assertEqual
          "testList - Should match single number in list with trail"
          (S.MatchOk (BLU.fromString "  ") "[1]")
          (S.matchList . BLU.fromString $ "[1]  "))
    , TestCase
        (assertEqual
          "testList - Should match three number"
          (S.MatchOk BL.empty "[1, 2, 3]")
          (S.matchList . BLU.fromString $ "[1, 2, 3]"))
    , TestCase
        (assertEqual
          "testList - Should match three string"
          (S.MatchOk BL.empty "[\"Deus\", \"Ex\", \"Machina\"]")
          (S.matchList . BLU.fromString $ "[\"Deus\", \"Ex\", \"Machina\"]"))
    , TestCase
        (assertEqual
          "testList - Should match nested empty lists"
          (S.MatchOk BL.empty "[[[]]]")
          (S.matchList . BLU.fromString $ "[[[]]]"))
    , TestCase
        (assertEqual
          "testList - Should match nested lists"
          (S.MatchOk BL.empty "[[[]]]")
          (S.matchList . BLU.fromString $ "[[[]]]"))
    , TestCase
        (assertEqual
          "testList - Should match three objects"
          (S.MatchOk BL.empty "[{\"name\": \"Nikhil\"}, {\"name\": \"Kalyani\"}, {\"name\": \"Shriom\"}]")
          (S.matchList . BLU.fromString $ "[{\"name\": \"Nikhil\"}, {\"name\": \"Kalyani\"}, {\"name\": \"Shriom\"}]"))
    ]

testNotChar =
  TestList
    [ TestCase
        (assertEqual
          "testNotChar - Should escape on end stream"
          (S.MatchEnd "")
          ((S.matchNotChar '"') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testNotChar - Should error on double quote"
          (S.MatchErr (BLU.fromString "\"") "Not expected but found it")
          ((S.matchNotChar '"') . BLU.fromString $ "\""))
    , TestCase
        (assertEqual
          "testNotChar - Should match on space"
          (S.MatchOk BL.empty " ")
          ((S.matchNotChar '"') . BLU.fromString $ " "))
    ]

testPeek =
  TestList
    [ TestCase
        (assertEqual
          "testPeek - Should escape on end stream"
          (S.MatchEnd "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testPeek - Should error on peeking 'a'"
          (S.MatchErr (BLU.fromString "a ") "Found a instead of  ")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "testPeek - Should match on single ' ' without trail"
          (S.MatchOk (BLU.fromString " ") "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "testPeek - Should match on single ' ' with trail"
          (S.MatchOk (BLU.fromString " a") "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ " a"))
    , TestCase
        (assertEqual
          "testPeek - Should match on single '  ' with ' ' as trail"
          (S.MatchOk (BLU.fromString "  ") "")
          ((S.peek $ S.matchChar ' ') . BLU.fromString $ "  "))
    ]

testPeekNot =
  TestList
    [ TestCase
        (assertEqual
          "testPeekNot - Should escape on end stream"
          (S.MatchEnd "")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testPeekNot - Should match on peeking 'a'"
          (S.MatchOk (BLU.fromString "a ") "")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "testPeekNot - Should error on single ' ' without trail"
          (S.MatchErr (BLU.fromString " ") "Not expected but found it")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "testPeekNot - Should error on double ' ' without trail"
          (S.MatchErr (BLU.fromString "  ") "Not expected but found it")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ "  "))
    , TestCase
        (assertEqual
          "testPeekNot - Should match on double 'b' without trail"
          (S.MatchOk (BLU.fromString "bb") "")
          ((S.peek $ S.matchNotChar ' ') . BLU.fromString $ "bb"))
    ]

testOr =
  TestList
    [ TestCase
        (assertEqual
          "testOr - Should escape on end stream"
          (S.MatchEnd "")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testOr - Should match on 'a'"
          (S.MatchOk (BLU.fromString "bc") "a")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ "abc"))
    , TestCase
        (assertEqual
          "testOr - Should match on 'b'"
          (S.MatchOk (BLU.fromString "ba") "b")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ "bba"))
    , TestCase
        (assertEqual
          "testOr - Should error on 'c'"
          (S.MatchErr (BLU.fromString "ccc") "Found c instead of b")
          (((S.matchChar 'a') `S.matchOr` (S.matchChar 'b')) . BLU.fromString $ "ccc"))
    ]

testZeroOrMoreChar =
  TestList
    [ TestCase
        (assertEqual
          "testZeroOrMoreChar - Should escape on end stream"
          (S.MatchEnd "")
          ((S.matchZeroOrMoreChar $ S.matchChar 'a') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testZeroOrMoreChar - Should match on no 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "")
          ((S.matchZeroOrMoreChar $ S.matchChar 'a') . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "testZeroOrMoreChar - Should match on single 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "a")
          ((S.matchZeroOrMoreChar $ S.matchChar 'a') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "testZeroOrMoreChar - Should match on single 'a' without trail"
          (S.MatchEnd "a")
          ((S.matchZeroOrMoreChar $ S.matchChar 'a') . BLU.fromString $ "a"))
    , TestCase
        (assertEqual
          "testZeroOrMoreChar - Should match on triple 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "aaa")
          ((S.matchZeroOrMoreChar $ S.matchChar 'a') . BLU.fromString $ "aaa "))
    , TestCase
        (assertEqual
          "testZeroOrMoreChar - Should match on triple 'a' without trail"
          (S.MatchEnd "aaa")
          ((S.matchZeroOrMoreChar $ S.matchChar 'a') . BLU.fromString $ "aaa"))
    ]

testObject =
  TestList
    [ TestCase
        (assertEqual
          "testObject - Should escape on end stream"
          (S.MatchEnd "")
          (S.matchObject . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testObject - Should error out on space"
          (S.MatchErr (BLU.fromString " ") "Found   instead of {")
          (S.matchObject . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "testObject - Should match empty object"
          (S.MatchOk BL.empty "{}")
          (S.matchObject . BLU.fromString $ "{}"))
    , TestCase
        (assertEqual
          "testObject - Should match pair in object"
          (S.MatchOk BL.empty "{\"key\": 42}")
          (S.matchObject . BLU.fromString $ "{\"key\": 42}"))
    , TestCase
        (assertEqual
          "testObject - Should match pair in object with spaces in between"
          (S.MatchOk (BLU.fromString "    ") "{    \"key\"  :  42 }")
          (S.matchObject . BLU.fromString $ "{    \"key\"  :  42 }    "))
    , TestCase
        (assertEqual
          "testObject - Should match single number in object with trail"
          (S.MatchOk (BLU.fromString "  ") "{\"key\": 42}")
          (S.matchObject . BLU.fromString $ "{\"key\": 42}  "))
    , TestCase
        (assertEqual
          "testObject - Should match three pairs in object"
          (S.MatchOk BL.empty "{\"proton\": 1, \"neutron\": 0, \"electron\": 1}")
          (S.matchObject . BLU.fromString $ "{\"proton\": 1, \"neutron\": 0, \"electron\": 1}"))
    , TestCase
        (assertEqual
          "testObject - Should match nested objects"
          (S.MatchOk BL.empty "{\"deep\": {\"deeper\": {\"deepest\": 0}}}")
          (S.matchObject . BLU.fromString $ "{\"deep\": {\"deeper\": {\"deepest\": 0}}}"))
    ]

testZeroOrOneChar =
  TestList
    [ TestCase
        (assertEqual
          "testZeroOrOneChar - Should escape on end stream"
          (S.MatchEnd "")
          ((S.matchZeroOrOneChar $ S.matchChar 'a') . BLU.fromString $ ""))
    , TestCase
        (assertEqual
          "testZeroOrOneChar - Should match on no 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "")
          ((S.matchZeroOrOneChar $ S.matchChar 'a') . BLU.fromString $ " "))
    , TestCase
        (assertEqual
          "testZeroOrOneChar - Should match on single 'a' with trail"
          (S.MatchOk (BLU.fromString " ") "a")
          ((S.matchZeroOrOneChar $ S.matchChar 'a') . BLU.fromString $ "a "))
    , TestCase
        (assertEqual
          "testZeroOrOneChar - Should match on single 'a' without trail"
          (S.MatchOk BL.empty "a")
          ((S.matchZeroOrOneChar $ S.matchChar 'a') . BLU.fromString $ "a"))
    , TestCase
        (assertEqual
          "testZeroOrOneChar - Should match on single 'a' on double 'a'"
          (S.MatchOk (BLU.fromString "a") "a")
          ((S.matchZeroOrOneChar $ S.matchChar 'a') . BLU.fromString $ "aa"))
    ]

testSuite =
  TestList
    [ testNot
    , testNotChar
    , testPeek
    , testOr
    , testPeekNot
    , testZeroOrMoreChar
    , testZeroOrOneChar
    , testInteger
    , testFloat
    , testString
    , testList
    , testObject
    ]

main :: IO Counts
main = runTestTT testSuite
