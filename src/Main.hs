import Structures
import Parser
import Rules
import Schemes
import Control.Exception
import System.Timeout
import Examples
import Data.List
-- import Tests

main :: IO ()
main = mainMenu

centerText :: Int -> String -> String
centerText width text =
  let padding = replicate ((width - length text) `div` 2) ' '
  in padding ++ text

mainMenu = do
  let width = 80 
  putStrLn (replicate width '-') 
  putStrLn (centerText width "*** Welcome to the Deep Inference theorem prover! ***")
  putStrLn (replicate width '-')  
  putStrLn ""
  putStrLn ""
  putStrLn (centerText width "Deep inference is a methodology for designing proof theoretical formalisms,")
  putStrLn (centerText width "in which inference rules can be applied arbitrarily deeply inside a formula.")
  putStrLn ""
  putStrLn ""
  putStrLn (centerText width "This theorem prover is based on System SKS - a classical propositional logic")
  putStrLn (centerText width "system in calculus of structures. Explore each functions to understand more")
  putStrLn (centerText width "about how inference rules are applied, and how a proofs are constructed.")
  putStrLn (centerText width "You can use the examples, or define your own formula.")
  putStrLn ""
  putStrLn ""
  putStrLn (centerText width "Have fun!")
  putStrLn ""
  putStrLn ""
  putStrLn (replicate width '-')
  putStrLn ""
  putStrLn " Please select an option:" 
  putStrLn " 1. Automated Proof Search"
  putStrLn " 2. Interactive Proof Search"
  putStrLn " 3. Exit"
  putStrLn ""
  putStrLn (replicate width '-') 
  option <- getLine
  case option of
    "1" -> automatedProofSearch
    "2" -> interactiveProofSearch
    "3" -> putStrLn "Bye!"
    _   -> do
      putStrLn "Invalid option. Please try again."
      mainMenu  

automatedProofSearch :: IO ()
automatedProofSearch = do
  let width = 80 
  putStrLn (replicate width '-') 
  putStrLn ""
  putStrLn " Please select an option:"    
  putStrLn " 1. Define own formula"
  putStrLn " 2. Select from examples"
  putStrLn " 3. Back to main menu"
  putStrLn ""
  putStrLn (replicate width '-') 
  userDefined <- getLine
  case userDefined of
    "1" -> do
      putStrLn " Type in your formula here: "
      struct <- getLine
      formula <- try (evaluate (parseStruct struct)) :: IO (Either SomeException Struct)
      case formula of
        Left _ -> putStrLn "Invalid formula entered." >> automatedProofSearch
        Right val -> do
          putStrLn "\n\n"
          timeout 6000000 (putStr (formatOutput (fst (testFunctionWithTree bfsStrictNoRepeat ((clean . removeImpEqu) (val)))))) >> return ()
    "2" -> do
      formula <- chooseExample
      putStr "\n\n" >> timeout 6000000 (putStr (formatOutput (fst (testFunctionWithTree bfsStrictNoRepeat ((clean . removeImpEqu) (formula)))))) >> return ()
    "3" -> mainMenu
    _   -> do
      putStrLn "Invalid option. Please try again."
      automatedProofSearch 

interactiveProofSearch :: IO ()
interactiveProofSearch = do
  let width = 80 
  putStrLn (replicate width '-') 
  putStrLn ""
  putStrLn " 1. Define own formula"
  putStrLn " 2. Select from examples"
  putStrLn " 3. Back to main menu"
  putStrLn ""
  putStrLn (replicate width '-') 
  userDefined <- getLine
  case userDefined of
    "1" -> do
      putStrLn " Type in your formula here: "
      struct <- getLine
      formula <- try (evaluate (parseStruct struct)) :: IO (Either SomeException Struct)
      case formula of
        Left _ -> putStrLn "Invalid formula entered." >> interactiveProofSearch  
        Right val -> transform ((clean . removeImpEqu) (parseStruct struct)) 
    "2" -> do    
      formula <- chooseExample
      transform ((clean . removeImpEqu) formula)
    "3" -> mainMenu
    _   -> do
      putStrLn "Invalid option. Please try again."
      interactiveProofSearch 
examples = [
  ("Statman Tautology S1", s1),
  ("Statman Tautology S2", s2),
  ("Law of Composition", pc3),
  ("Law of Adjunction", pc4),
  ("Modus Ponens", modusPonens),
  ("Reductio Ad Absurdum", reductio)]

chooseExample :: IO Struct
chooseExample = do
  let width = 80 
  putStrLn (replicate width '-') 
  putStrLn ""
  putStrLn " Please choose your example tautology:"
  putStrLn " 1. Statman Tautology S1"
  putStrLn " 2. Statman Tautology S2"
  putStrLn " 3. Law of Composition"
  putStrLn " 4. Law of Adjunction"
  putStrLn " 5. Modus Ponens"
  putStrLn " 6. Reductio Ad Absurdum"
  putStrLn ""
  putStrLn (replicate width '-') 
  choice <- getLine
  putStrLn ((fst (examples !! ((read choice) - 1))) ++ ": " ++ show (snd (examples !! ((read choice) - 1))))
  putStrLn (replicate width '-') 
  return (snd (examples !! ((read choice) - 1)))

formatTransform :: Int -> [(String, Struct)] -> String
formatTransform _ [] = ""
formatTransform i ((r,x):xs) =
  " " ++ (show i) ++ " (" ++ r ++ ") : " ++ (show x) ++ "\n" ++ formatTransform (i + 1) xs

transform :: Struct -> IO ()
transform x = do
    transformLoop [("Start",x)]

transformLoop :: [(String, Struct)] -> IO ()
transformLoop xs = do
  let list = transformationStep (snd (last xs))
  if (length list) == 0 then do
    putStrLn "Done !"
    putStr "\n\n" >> putStr( formatOutput xs )
  else do
    putStrLn ("\nCurrent: " ++ show (snd (last xs)) ++ "\n")
    putStr (formatTransform 1 list)
    putStr "\nChoice: "
    selection <- getLine
    transformLoop (xs ++ [list !! ((read selection) - 1)])


