import Test.HUnit
import qualified GrandestWords

main = do
  let test1 = TestCase $ assertEqual
                         "Should return grandest words"
                         (["grandest"], ["word."])
                         (GrandestWords.getGrandestWords "A grandest word.")

  let test2 = TestCase $ assertEqual
                         "Should ignore case"
                         (["Kick", "you,", "will"], ["I"])
                         (GrandestWords.getGrandestWords "Kick you, I will")

  let test3 = TestCase $ assertEqual
                       "Should ignore numbers and special characters"
                       (["and", "C-3PO"], ["R2-D2"])
                       (GrandestWords.getGrandestWords "R2-D2 and C-3PO")

  let tests = TestList [ "test1" ~: test1
                       , "test2" ~: test2
                       , "test3" ~: test3 ]
  runTestTT tests
