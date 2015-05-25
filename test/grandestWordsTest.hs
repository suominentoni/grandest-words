import Test.HUnit
import qualified GrandestWords

main = do
  let test1 = TestCase $ assertEqual "Should return grandest words" (["aabbccdd"], ["aabbcc"]) (GrandestWords.getGrandestWords ["aa", "aabb", "aabbcc", "aabbccdd"])
  let test2 = TestCase $ assertEqual "Should ignore case" (["abcC", "abc"], ["a"]) (GrandestWords.getGrandestWords ["abc", "abcC", "a"])
  let tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
  runTestTT tests
