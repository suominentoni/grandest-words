import Test.HUnit
import qualified GrandestWords

main = do
  runTestTT $ TestList [ "test1" ~: assertEqual
                                     "Should return grandest word pairs"
                                     ([("you", "bet"), ("you", "and"), ("you", "plr"), ("bet", "and"), ("bet", "plr"), ("and", "plr")], 6)
                                     (GrandestWords.getGrandestPairs "qw you bet gh and ui plr")
                        , "test2" ~: assertEqual
                                     "Should return grandest word pairs"
                                     ([("craftsmen", "Dwarf-Lords,"), ("Dwarf-Lords,", "mountain")], 13)
                                     (GrandestWords.getGrandestPairs "Seven to the Dwarf-Lords, great miners and craftsmen of the mountain halls")
                        , "test3" ~: assertEqual
                                     "Should ignore case"
                                     ([("shout", "SHOUT")], 5)
                                     (GrandestWords.getGrandestPairs "shout SHOUT")
                        , "test4" ~: assertEqual
                                     "Should ignore special characters and numbers"
                                     ([("C3-PO", "R2-D2") ], 5)
                                     (GrandestWords.getGrandestPairs "R2-D2 C3-PO") ]

