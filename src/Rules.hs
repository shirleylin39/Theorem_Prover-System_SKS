module Rules where
import Structures
import Data.List 
import Control.Exception (allowInterrupt)
import System.Console.Terminfo (cursorDown, restoreDefaultColors)

------- *** Inference Rules *** -------

aiDown :: Struct -> Struct
aiDown (Disj a) =
    let atoms = openAtoms a
        selfAtoms = intersect atoms (negAll atoms)
        (updatedStruct, pairRemoved) = removeFirstPair a selfAtoms
        result = if pairRemoved
                    then Disj (updatedStruct ++ [T])
                    else Disj (map aiDown updatedStruct)
    in toSingleton result
    where openAtoms a = [x | x <- a, isAtom x]
          removeFirstPair :: [Struct] -> [Struct] -> ([Struct], Bool)
          removeFirstPair xs [] = (xs, False)
          removeFirstPair xs (atom:rest) =
              if atom `elem` xs && Neg atom `elem` xs 
              then let xs' = delete atom xs
                       xs'' = delete (Neg atom) xs'
                   in (xs'', True)
              else removeFirstPair xs rest
aiDown a = a

aiWeakDown :: Struct -> Struct
aiWeakDown (Disj a)
    | intersect (openAtoms a) (negAll (openAtoms a)) /= [] = T
    | otherwise = Disj (map aiWeakDown a)
    where openAtoms a = [x | x <- a, isAtom x]
aiWeakDown a = a

acDown :: Struct -> Struct
acDown (Atom a) = Disj [Atom a, Atom a]
acDown (Neg (Atom a)) = Disj [Neg (Atom a), Neg (Atom a)]
acDown a = a

awDown :: Struct -> Struct
awDown (Atom a) = F
awDown (Neg (Atom a)) = F
awDown a = a

fDown :: Struct -> Struct
fDown (Disj a) = toSingleton (Disj (filter (/= F) a))
fDown a = a

tDown :: Struct -> Struct
tDown (Conj a) = toSingleton (Conj (filter (/= T) a))
tDown a = a

-- acUp --

acUp :: Struct -> Struct
acUp (Conj a) = toSingleton (Conj (nubBy equalAtom  a)) 
acUp a = a

acUpGroup :: Struct -> Struct
acUpGroup (Conj a) = toSingleton( groupConjunction (map acUpGroup a) )
acUpGroup (Disj a) = Disj (map acUpGroup a)
acUpGroup a = a 

groupConjunction :: [Struct] -> Struct
groupConjunction atoms = Conj (map groupOrKeep (groupAtoms atoms))

groupAtoms :: [Struct] -> [[Struct]]
groupAtoms = groupBy equalAtom . sortAtoms

groupOrKeep :: [Struct] -> Struct
groupOrKeep [x] = x 
groupOrKeep xs  = Conj xs 

sortAtoms :: [Struct] -> [Struct]
sortAtoms = map sortInside . sortBy compareAtoms

sortInside :: Struct -> Struct
sortInside (Conj xs) = Conj (sortAtoms xs)
sortInside (Disj xs) = Disj (sortAtoms xs)
sortInside x = x 

compareAtoms :: Struct -> Struct -> Ordering
compareAtoms (Atom x) (Atom y) = compare x y
compareAtoms _ _ = EQ 

------- *** Helper Functions for Switch and Medial *** -------

-- Finds all permutations
findPermutations :: Struct -> [Struct]
findPermutations (Disj a) = [Disj px | px <- permutations a]
findPermutations (Conj a) = [Conj px | px <- permutations a]

-- Finds all partitions
findPartitions :: Struct -> [(Struct, Struct)]
findPartitions (Disj a) = [(Disj (take i a), Disj (drop i a)) | i <- [1..(length a)]]
findPartitions (Conj a) = [(Conj (take i a), Conj (drop i a)) | i <- [1..(length a - 1)]]

-- Counts the number of elements of a disjunction or conjunction that are not T or F
noNonUnit :: Struct -> Int
noNonUnit (Disj a) = length [x | x <- a, x /= F, x /= T]
noNonUnit (Conj a) = length [x | x <- a, x /= F, x /= T]
noNonUnit a        = 0

-- Checks if a structure is a proper disjunction
properDisj :: Struct -> Bool
properDisj (Disj a) = noNonUnit (Disj a) >= 2
properDisj a        = False

-- Checks if a structure is a proper conjunction
properConj :: Struct -> Bool
properConj (Conj a) = noNonUnit (Conj a) >= 2
properConj a        = False

------- *** Switch Rule *** ------- 

--  Extract one conjunction from a disjunction
extractConjs :: Struct -> [(Struct, Struct)]
extractConjs (Disj xs) = [(conj, Disj (delete conj xs)) | conj <- filter isConj xs]
  where
    isConj (Conj _) = True
    isConj _ = False
extractConjs _ = []

-- Find all permutations and partitions
switchPermutationPartition :: (Struct, Struct) -> [((Struct, Struct), (Struct, Struct))]
switchPermutationPartition (a, b) = [(as, bs) | as <- switchPP a, bs <- switchPP b]
  where
    switchPP x = concatMap findPartitions (findPermutations x)

-- Finds all combinations of partitions and permutations
switchAllComb :: Struct -> [((Struct, Struct), (Struct, Struct))]
switchAllComb a = concatMap switchPermutationPartition (extractConjs a)

----  Constraints (dlis) ---
-- Filters improper conjunctions and disjunctions
filterProperSwitch :: [((Struct, Struct), (Struct, Struct))] -> [((Struct, Struct), (Struct, Struct))]
filterProperSwitch xs = filter notProperSwitch xs
  where
    notProperSwitch ((r, t), (ua, ub)) = not (properDisj ua) && not (properConj r)

-- Filters legal intersections
filterLegalIntersectionSwitch :: [((Struct, Struct), (Struct, Struct))] -> [((Struct, Struct), (Struct, Struct))]
filterLegalIntersectionSwitch xs = filter legalIntersectionSwitch xs
  where
    legalIntersectionSwitch ((r, t), (ua, ub)) = intersect (negAll (allAtoms r)) (allAtoms ua) /= []

-- Finds legal combinations for Switch 
findLegalSwitch :: Struct -> [((Struct, Struct), (Struct, Struct))]
findLegalSwitch a = filterLegalIntersectionSwitch (filterProperSwitch (switchAllComb (clean a)))

-- Apply switch rule
applySwitch :: ((Struct, Struct), (Struct, Struct)) -> Struct
applySwitch ((r, t), (ua, Disj [])) = clean (Conj [Disj [r, ua], t])
applySwitch ((r, t), (ua, ub)) = clean (Disj [Conj [Disj [r, ua], t], ub])

-- Apply switch rule at deeepest position
applyDeepestSwitch :: ((Struct, Struct), (Struct, Struct)) -> [Struct]
applyDeepestSwitch ((r, t), (ua, ub))
  | findLegalSwitch (clean r) /= [] = [clean (Disj [Conj [r1, t], ua, ub]) | r1 <- switch r]
  | findLegalSwitch (clean ua) /= [] = [clean (Disj [Conj [r, t], ua1, ub]) | ua1 <- switch ua]
  | otherwise = [applySwitch ((r, t), (ua, ub))]

switch :: Struct -> [Struct]
switch (Disj a)
  | findLegalSwitch (Disj a) == [] = [Disj a]
  | otherwise = concatMap applyDeepestSwitch (findLegalSwitch (Disj a))
switch a = [a]

switchList :: Struct -> [Struct]
switchList (Disj a) = concatMap applyDeepestSwitch (findLegalSwitch (Disj a))
switchList _ = []

------- *** Medial Rule *** ------

--  Extract two disjunctions from a Ccnjunction
extractTwoDisj :: Struct ->  [(Struct, Struct, Struct)]
extractTwoDisj (Conj a) = 
    let disjs = filter isDisj a 
        pairs = [(d1, d2, Conj (delete d1 (delete d2 a))) | (d1:rest) <- tails disjs, d2 <- rest]
    in pairs
    where 
      isDisj (Disj _) = True
      isDisj _ = False
extractTwoDisj _ = []

-- Find all permutations and partitions
medialPermutePartition :: (Struct, Struct, Struct) -> [((Struct , Struct), (Struct , Struct), Struct)]
medialPermutePartition (a, b, c) = 
    [(as, bs, c) | as <- medialPP a, bs <- medialPP b]
  where
    medialPP x = concatMap findPartitions (findPermutations x)

-- Finds all combinations
medialAllComb :: Struct ->  [((Struct , Struct), (Struct , Struct), Struct)]
medialAllComb a = concatMap medialPermutePartition (extractTwoDisj a)

---- Constraints (similar concepts to dlis) ----
-- Filters improper conjunctions and disjunctions
filterProperMedial :: [((Struct, Struct), (Struct, Struct), Struct)] -> [((Struct, Struct), (Struct, Struct), Struct)]
filterProperMedial xs = filter notProperMedial xs
  where
    notProperMedial ((r, t), (u, v), x) = not (properDisj u) && not (properDisj r)

-- Filters legal intersections
filterLegalIntersectionMedial :: [((Struct, Struct), (Struct, Struct), Struct)] -> [((Struct, Struct), (Struct, Struct), Struct)]
filterLegalIntersectionMedial xs = filter legalIntersectionMedial xs
  where
    legalIntersectionMedial ((r, t), (u, v), x) = intersect (allAtoms r) (allAtoms u) /= []

-- Finds legal combinations for medial
findLegalMedial :: Struct -> [((Struct, Struct), (Struct, Struct), Struct)]
findLegalMedial a = filterLegalIntersectionMedial (filterProperMedial (medialAllComb (clean a)))

-- Applies medial rules to a structure
applyMedial :: ((Struct, Struct), (Struct,Struct), Struct) -> Struct
applyMedial ((r, t), (u, v), Conj []) = 
    clean (Disj [Conj [r, u], Conj [t, v]])
applyMedial ((r, t), (u, v), x) = 
    clean (Conj [Disj [Conj [r, u], Conj [t, v]], x])

-- Apply medial rule at deeepest position
applyDeepestMedial :: ((Struct, Struct), (Struct, Struct), Struct) -> [Struct]
applyDeepestMedial ((r, t), (u, v), x)
  | findLegalMedial  (clean r) /= [] = 
      [clean (Conj [Disj [r1, t], Disj [u, v], x]) | r1 <- medial r]
  | findLegalMedial  (clean u) /= [] = 
      [clean (Conj [Disj [r, t], Disj [u1, v], x]) | u1 <- medial u]
  | otherwise = [applyMedial ((r, t), (u, v), x)]

medial :: Struct -> [Struct]
medial (Conj a)
  | null (findLegalMedial(Conj a)) = [Conj a]
  | otherwise = concatMap applyDeepestMedial (findLegalMedial  (Conj a))
medial a = [a]

medialList :: Struct -> [Struct]
medialList (Conj a) = concatMap applyDeepestMedial (findLegalMedial  (Conj a))
medialList _ = []


----- *** DFS for applying rules to all subformulae *** -----
depthFirstSearch :: (Struct -> Struct) -> Struct -> Struct
depthFirstSearch f (Disj x)
  | f (Disj x) /= Disj x = f (Disj x)
  | otherwise = Disj (depthFirstSearchList f x)
depthFirstSearch f (Conj x)
  | f (Conj x) /= Conj x = f (Conj x)
  | otherwise = Conj (depthFirstSearchList f x)
depthFirstSearch f a = f a

depthFirstSearchList :: (Struct -> Struct) -> [Struct] -> [Struct]
depthFirstSearchList f [] = []
depthFirstSearchList f (x:xs)
  | depthFirstSearch f x /= x = depthFirstSearch f x : xs
  | otherwise = x : depthFirstSearchList f xs

-- Applies a function after a certain number of DFS steps
dfsToNumber :: (Struct -> Struct) -> Struct -> Int -> Struct
dfsToNumber f x 0 = f x
dfsToNumber f (Disj x) i = Disj [dfsToNumber f a b | (a, b) <- zip x (computeDFSNumbers i x)]
dfsToNumber f (Conj x) i = Conj [dfsToNumber f a b | (a, b) <- zip x (computeDFSNumbers i x)]
dfsToNumber _ a _ = a

computeDFSNumbers :: Int -> [Struct] -> [Int]
computeDFSNumbers j xs = [j - (1 + sum (take i tsx)) | i <- [0..((length tsx) - 1)]]
  where
    tsx = [treeSize x | x <- xs]

treeSize :: Struct -> Int
treeSize (Disj x) = 1 + sum [treeSize a | a <- x]
treeSize (Conj x) = 1 + sum [treeSize a | a <- x]
treeSize (Neg (Atom a)) = 2
treeSize _ = 1

-- Recurses down to a number and returns only that substructure
dfsToNumberNonRecursive :: Struct -> Int -> Struct
dfsToNumberNonRecursive x i = case dfs2 x i of
    []     -> x 
    (y:ys) -> y
  where
    dfs2 :: Struct -> Int -> [Struct]
    dfs2 x 0 = [x]
    dfs2 (Disj x) i = concat [dfs2 a b | (a, b) <- (zip x (computeDFSNumbers i x)) ]
    dfs2 (Conj x) i = concat [dfs2 a b | (a, b) <- (zip x (computeDFSNumbers i x)) ]
    dfs2 a i = []


--- *** DFS of inference rules *** ---
listAll :: Struct -> (Struct -> Struct) -> [Struct] --aiDown, aiWeakDown, acDown, fDown, tDown
listAll x f = map assoc (delete x (nub (map (dfsToNumber f x) [0..((treeSize x)-1)])))

listAllacUp :: Struct -> [Struct]
listAllacUp x = listAll (acUpGroup x) acUp

listAllSwitch :: Struct -> [Struct]
listAllSwitch x = map clean (delete x (nub (concatMap (listSwitchforSubStruct x) [0..((treeSize x) - 1)])))

listSwitchforSubStruct :: Struct -> Int -> [Struct]
listSwitchforSubStruct x i = nub [dfsToNumber (\y -> ls !! j) x i | j <- [0..(length ls) - 1]]
  where
    ls = switchList (dfsToNumberNonRecursive x i)

listAllMedial :: Struct -> [Struct]
listAllMedial x = map clean (delete x (nub (concatMap (listMedialforSubStruct x) [0..((treeSize x) - 1)])))

listMedialforSubStruct :: Struct -> Int -> [Struct]
listMedialforSubStruct x i = nub [dfsToNumber (\y -> ls !! j) x i | j <- [0..(length ls) - 1]]
  where
    ls = medialList (dfsToNumberNonRecursive x i)