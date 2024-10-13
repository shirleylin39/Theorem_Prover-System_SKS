module Schemes where

import Rules
import Structures
import Data.List
import Examples
import Distribution.Simple.Utils (xargs)

data ProofTree = ProofTree {rule :: String, struct :: Struct, rest :: [ProofTree]} 
              | Empty 
              | End 
              deriving (Show, Eq)

-- Proof steps to test
proofStep ::  (Struct -> [ProofTree]) -> ProofTree -> ProofTree
proofStep f (ProofTree r s [End]) =  ProofTree r s [End]
proofStep f (ProofTree r s [Empty]) = ProofTree r s (f s)
proofStep f (ProofTree r s xs) = ProofTree r s (map (proofStep f) xs)


---- *** Schemes *** ----
-- Breadth-First Search
bfsProofStep :: ProofTree -> ProofTree
bfsProofStep = proofStep (listToTree . listAllRules)

-- Depth-First Search
dfsProofStep :: ProofTree -> ProofTree
dfsProofStep = proofStep dfsStep
dfsStep :: Struct -> [ProofTree]
dfsStep x = (listToTree . pickFirst) (listNonSwitchMedialRules x ++ labelStructs "s" (listAllSwitch x) ++ labelStructs "m" (listAllMedial x))
-- 添加標籤並將 Struct 列表轉換成 [(String, Struct)]
labelStructs :: String -> [Struct] -> [(String, Struct)]
labelStructs _ [] = []  -- 如果列表為空，返回空列表
labelStructs label structs = [(label, s) | s <- structs]  -- 為每個 Struct 添加標籤
pickFirst :: [a] -> [a]
pickFirst xs
  | null xs = []
  | otherwise = [xs !! 0]

-- Depth-First Search prefering switch and aiw Rule
dfsSwitchAiw :: ProofTree -> ProofTree
dfsSwitchAiw = proofStep (listToTree . pickFirstSwitchAiw . listAllRules)
pickFirstSwitchAiw :: [(String, Struct)] -> [(String, Struct)]
pickFirstSwitchAiw xs 
  | null xs = []
  | fst (xs !! 0) == "s" && not (null ([x | x <- xs, not (null (listAll (snd x) aiWeakDown))])) = [[x | x <- xs, not (null (listAll (snd x) aiWeakDown))] !! 0]
  | otherwise = [xs !! 0]

-- Depth-First Search prefering medial and acUp Rule
dfsMedialAcup:: ProofTree -> ProofTree
dfsMedialAcup = proofStep (listToTree . pickFirstMedialAcup . listAllRules)
pickFirstMedialAcup :: [(String, Struct)] -> [(String, Struct)]
pickFirstMedialAcup xs 
  | null xs = []
  | fst (xs !! 0) == "m" && not (null ([x | x <- xs, not (null (listAllacUp (snd x)))])) = [[x | x <- xs, not (null (listAllacUp (snd x)))] !! 0]
  | otherwise = [xs !! 0]

-- Depth-First Search with restricted awDown
dfsWithRestrictedAwDown :: ProofTree -> ProofTree
dfsWithRestrictedAwDown = proofStep (listToTree . awDownRestriction . listAllRules)
awDownRestriction :: [(String, Struct)] -> [(String, Struct)]
awDownRestriction xs
  | null xs = [] 
  | fst (xs !! 0) == "aw↓" && null (listAll (snd (xs !! 0)) fDown) = tail xs
  | otherwise = [xs !! 0] 

-- Depth-First Search with restricted acDown
dfsWithRestrictedAcDown :: ProofTree -> ProofTree
dfsWithRestrictedAcDown = proofStep (listToTree . acDownRestriction . listAllRules)
acDownRestriction :: [(String, Struct)] -> [(String, Struct)]
acDownRestriction xs
  | null xs = [] 
  | fst (xs !! 0) == "ac↓" && null (listAllSwitch (snd (xs !! 0))) = tail xs
  | otherwise = [xs !! 0]  

-- Depth-First Search strict pick
dfsStrict :: ProofTree -> ProofTree
dfsStrict = proofStep (listToTree . strictPick . listAllRules)
strictPick :: [(String, Struct)] -> [(String, Struct)]
strictPick xs
  | null xs = []
  | fst (xs !! 0) == "s" && not (null [x | x <- xs, not (null (listAll (snd x) aiWeakDown))]) = [[x | x <- xs, not (null (listAll (snd x) aiWeakDown))] !! 0]
  | fst (xs !! 0) == "m" && not (null [x | x <- xs, not (null (listAllacUp (snd x)))]) = [[x | x <- xs, not (null (listAllacUp (snd x)))] !! 0]
  | fst (xs !! 0) == "aw↓" && null (listAll (snd (xs !! 0)) fDown) = tail xs
  | fst (xs !! 0) == "ac↓" && null (listAllSwitch (snd (xs !! 0))) = tail xs
  | otherwise = [xs !! 0]

-- Breadth-First Search strict pick
bfsStrict :: ProofTree -> ProofTree
bfsStrict = proofStep (listToTree . filterStrictRules . listAllRules)
filterStrictRules :: [(String, Struct)] -> [(String, Struct)]
filterStrictRules xs
  | null xs = []
  | fst (xs !! 0) == "s" && null (listAll (snd (xs !! 0)) aiWeakDown) = filterStrictRules (tail xs)
  | fst (xs !! 0) == "m" && null (listAllacUp (snd (xs !! 0))) = filterStrictRules (tail xs)
  | fst (xs !! 0) == "aw↓" && null (listAll (snd (xs !! 0)) fDown) = filterStrictRules (tail xs)
  | fst (xs !! 0) == "ac↓" && null (listAllSwitch (snd (xs !! 0))) = filterStrictRules (tail xs)
  | otherwise = xs

noRepeatStep :: ProofTree -> ProofTree -> ProofTree
noRepeatStep t (ProofTree r s [End]) = ProofTree r s [End]
noRepeatStep t (ProofTree r s [Empty])
  | null (listAllRules s) = ProofTree r s []
  | any (\x -> member (snd x) t) (listAllRules s) = ProofTree r s [End]
  | otherwise = ProofTree r s (listToTree (removeRepeats t (listAllRules s)))
noRepeatStep t (ProofTree r s xs) = ProofTree r s (map (noRepeatStep t) xs)

-- Breadth-First Search strict pick with step-by-step no repeat
bfsStrictNoRepeat :: ProofTree -> ProofTree
bfsStrictNoRepeat t = noRepeatStep t (proofStep (listToTree . filterStrictRules . listAllRules) t)

-- Depth-First Search strict pick with step-by-step no repeat
dfsStrictNoRepeat :: ProofTree -> ProofTree
dfsStrictNoRepeat t = noRepeatStep t (proofStep (listToTree . strictPick . listAllRules) t)


-- Depth-First Search with backtracking
dfsProofStepWithBacktrack :: ProofTree -> ProofTree
dfsProofStepWithBacktrack t = dfsBacktrack t []

-- DFS with backtracking, exploring branches
dfsBacktrack :: ProofTree -> [ProofTree] -> ProofTree
dfsBacktrack t stack
  | isDone t = backtrack stack
  | hasMoreBranches t = exploreMoreBranches t stack
  | otherwise = dfsProofStep t
  where
    isDone (ProofTree _ s [End]) = True
    isDone _ = False
    hasMoreBranches (ProofTree _ _ xs) = length xs > 1
    hasMoreBranches _ = False
    backtrack :: [ProofTree] -> ProofTree
    backtrack [] = t 
    backtrack (p:ps) = dfsBacktrack p ps 
    exploreMoreBranches :: ProofTree -> [ProofTree] -> ProofTree
    exploreMoreBranches (ProofTree r s (firstBranch:restBranches)) stack = 
      dfsBacktrack firstBranch ((ProofTree r s restBranches) : stack)
    exploreMoreBranches _ _ = t
    


-- Functions to search for a proof
searchForProof :: (ProofTree -> ProofTree) -> ProofTree -> [(String, Struct)]
searchForProof f x
  | not (null (pathsToTrue x)) = pathsToTrue x !! 0
  | not (null (pathsToDone x)) = pathsToDone x !! 0
  | otherwise                  = searchForProof f (f x)

searchForProofBench :: (ProofTree -> ProofTree) -> ProofTree -> (Bool, [(String, Struct)])
searchForProofBench f x
  | not (null (pathsToTrue x)) = (True, pathsToTrue x !! 0)
  | not (null (pathsToDone x)) = (False, pathsToDone x !! 0)
  | otherwise                  = searchForProofBench f (f x)


searchForProofWithTree :: (ProofTree -> ProofTree) -> ProofTree -> ([(String, Struct)], ProofTree)
searchForProofWithTree f x
  | not (null (pathsToTrue x)) = (pathsToTrue x !! 0, x)
  | not (null (pathsToDone x)) = (pathsToDone x !! 0, x)
  | otherwise                  = searchForProofWithTree f (f x)


pathsToTrue :: ProofTree -> [[(String, Struct)]]
pathsToTrue End                = []
pathsToTrue Empty              = []
pathsToTrue (ProofTree r T _)  = [[(r,T)]]
pathsToTrue (ProofTree r s xs) = map ((r,s):)(concatMap pathsToTrue xs)

pathsToDone :: ProofTree -> [[(String, Struct)]]
pathsToDone End                = []
pathsToDone Empty              = []
pathsToDone (ProofTree r s []) = [[(r,s)]]
pathsToDone (ProofTree r s xs) = map ((r,s):)(concatMap pathsToDone xs)


-- Helper Funcitons
startTree :: Struct -> ProofTree
startTree x = ProofTree "Start" x [Empty]

listToTree :: [(String, Struct)] -> [ProofTree]
listToTree xs = map f xs
  where f (rule, struct) = ProofTree rule struct [Empty]

member :: Struct -> ProofTree -> Bool
member x (ProofTree r s xs) = equalComm s x || any (member x) xs
member _ End = False
member _ Empty = False


removeRepeats :: ProofTree -> [(a, Struct)] -> [(a, Struct)]
removeRepeats t xs = [x | x <- nubBy (\x y -> equalComm (snd x) (snd y)) xs, not (member (snd x) t)]






listAllReducingRules :: Struct -> [(String, Struct)]
listAllReducingRules x = concat [[(r, s) | s <- map clean (listAll x f)] | (r,f) <- [("t↓",tDown), ("f↓", fDown), ("ai↓", aiDown),("aiw↓",aiWeakDown)]] ++ [("ac↑", r) | r <- listAllacUp x]
listNonSwitchMedialRules x = listAllReducingRules x ++ [("aw↓", r) | r <- listAll x awDown] ++ [("ac↓", r) | r <- listAll x acDown]
listAllRules x = listNonSwitchMedialRules x ++ [("s",r) | r <- map clean (listAllSwitch x)] ++ [("m",r) | r <- map clean (listAllMedial x)]

eqmc :: (String , Struct) -> (String , Struct) -> Bool
eqmc (a,b) (c,d) = (a == c) && equalComm b d
transformationStep :: Struct -> [(String , Struct)]
transformationStep x = nubBy eqmc ([("f↓", y) | y <- listAll x fDown] ++ [("t↓", y) | y <- listAll x tDown] ++ [("ai↓", y) | y <- listAll x aiDown] ++ [("aiw↓", y) | y <- listAll x aiWeakDown] ++ [("ac↑", y) | y <- listAllacUp x] ++ [("aw↓", y) | y <- listAll x awDown] ++ [("ac↓", y) | y <- listAll x acDown] ++ [("m", y) | y <- listAllMedial x]) ++ [("s", y) | y <- listAllSwitch x] 




-- Functions for formatting output

maxLength :: [(String, Struct)] -> Int
maxLength xs = maximum [length (show (snd s)) | s <- xs]

offSet :: Int -> Int -> String
offSet m s = replicate (5 + (m - s) `div` 2) ' '

findLineLength :: Struct -> Struct -> String
findLineLength a b = replicate (maximum [length (show a), length (show b)]) '-'


formatOutput :: [(String, Struct)] -> String
formatOutput x = f (maxLength x) (reverse x)
  where
    f m ((r,s):[]) = offSet m (length (show s)) ++ "  " ++  show s ++ "\n\n\n"
    f m ((r,s):(r',s'):xs) = offSet m (length (show s)) ++ "  " ++ show s ++ "\n" 
                              ++ offSet m (length (findLineLength s s')) ++ r 
                              ++ findLineLength s s' ++ "\n" ++ f m ((r',s'):xs)


testFunctionWithTree :: (ProofTree -> ProofTree) -> Struct -> ([(String, Struct)], ProofTree)
testFunctionWithTree proofStep struct = 
  searchForProofWithTree proofStep (startTree ((clean . removeImpEqu) struct))

testFunction :: (ProofTree -> ProofTree) -> Struct -> ([(String, Struct)])
testFunction proofStep struct =
   searchForProof proofStep (startTree ((clean . removeImpEqu) struct))


testFunctionBench :: (ProofTree -> ProofTree) -> Struct -> (Bool, [(String, Struct)])
testFunctionBench proofStep struct =
   searchForProofBench proofStep (startTree ((clean . removeImpEqu) struct))