import Test.HUnit
import qualified GrandestWords

main = do
  runTestTT $ TestList [ "test1" ~: assertEqual
                                    "Should return grandest words"
                                    (["grandest"], ["word."])
                                    (GrandestWords.getGrandestWords "A grandest word.")
                        , "test2" ~: assertEqual
                                     "Should ignore case and duplicate letters"
                                     (["Kick", "you,", "will"], ["I"])
                                     (GrandestWords.getGrandestWords "Kick you, I will")
                        , "test3" ~: assertEqual
                                     "Should ignore numbers and special characters"
                                     (["and", "C-3PO"], ["R2-D2"])
                                     (GrandestWords.getGrandestWords "R2-D2 and C-3PO") ]
