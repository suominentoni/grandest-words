import Test.HUnit
import qualified GrandestWords

main = do
  runTestTT $ TestList [ "test1" ~: assertEqual
                                     "Should return grandest word pairs"
                                     [("you", "bet"), ("you", "and"), ("you", "plr"), ("bet", "and"), ("bet", "plr"), ("and", "plr")]
                                     (GrandestWords.getGrandestPairs "qw you bet gh and ui plr")
                        , "test2" ~: assertEqual
                                     "Should return grandest word pairs"
                                     [("Dwarf-Lords,", "craftsmen")]
                                     (GrandestWords.getGrandestPairs "Seven to the Dwarf-Lords, great miners and craftsmen of the mountain halls") ]
