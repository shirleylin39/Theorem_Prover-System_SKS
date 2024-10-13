import Criterion.Main
import Structures
import Schemes 
import Rules
import Examples
import Parser
import System.Random 
import System.Timeout (timeout)
import Control.DeepSeq
import Control.Exception (evaluate)
import Schemes 



allStructs :: [Struct]
allStructs = [
  s1, s2, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, 
  pc9, pc10, pc11, pc12, pc13, pc14, pc15, pc16, 
  pc17, pc18, pc19, pc20, pc21, mye1, mye2]

randomStructs :: IO [Struct]
randomStructs = do
  num <- randomRIO (2, 3) 
  structs <- sequence $ replicate num (randomRIO (0, length allStructs - 1))
  return [allStructs !! i | i <- structs]

randomCombine :: IO Struct
randomCombine = do
    structs <- randomStructs  
    op <- randomRIO (0 :: Int, 2 :: Int)  
    return $ case op of
        0 -> Conj structs     
        1 -> Disj structs    
        2 -> case structs of  
               [s1, s2] -> Imp s1 s2
               [s1, s2, s3] -> Imp s1 (Conj [s2, s3]) 

generateExamples :: Int -> IO [Struct]
generateExamples 0 = return []
generateExamples n = do
  example <- randomCombine
  rest <- generateExamples (n - 1)
  return (example : rest)

timeoutLimit :: Int
timeoutLimit = 10 * 10^6



generateBenchmarks :: [Struct] -> [Benchmark]
generateBenchmarks examples = 
    [ bgroup "Comparison between different proof search methods" 
        [ bench "BFS" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction bfsProofStep (examples !! i))))),
          bench "DFS" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsProofStep (examples !! i))))),
          bench "BFS Strict" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction bfsStrict  (examples !! i))))),
          bench "DFS Strict" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsStrict  (examples !! i))))),
          bench "DFS s/aiwDown" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsSwitchAiw  (examples !! i))))),
          bench "DFS m/acUp" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsMedialAcup  (examples !! i))))),
          bench "DFS awDown/f" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsWithRestrictedAwDown  (examples !! i))))),
          bench "DFS acDown/s" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsWithRestrictedAcDown  (examples !! i))))),
          bench "BFS NoRepeat" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction bfsStrictNoRepeat  (examples !! i))))),
          bench "DFS NoRepeat" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsStrictNoRepeat (examples !! i))))),
          bench "DFS Backtrack" $ nfIO (timeout timeoutLimit (evaluate (rnf (testFunction dfsProofStepWithBacktrack (examples !! i)))))
        ] | i <- [0..149]
    ]

main :: IO ()
main = do
    examples <- generateExamples 200
    defaultMain  (generateBenchmarks examples)