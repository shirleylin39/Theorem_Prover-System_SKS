--backup

aiDown :: Struct -> Struct
aiDown (Disj a)
    | intersect (openAtoms a) (negAll (openAtoms a)) /= [] = T
    | otherwise = Disj a
    where openAtoms a = [x | x <- a, isAtom x]
aiDown a = a


aiDown :: Struct -> Struct
aiDown (Disj a) = Disj (map processDisjPart a)
  where
    processDisjPart part
        | intersect (openAtoms [part]) (negAll (openAtoms [part])) /= [] = T
        | otherwise = part
        where openAtoms a = [x | x <- a, isAtom x]
aiDown a = a

cUp :: Struct -> Struct
cUp (Conj a) = toSingleton (Conj (nubBy equalComm a))
cUp a = a

-- Applies CS rules to a structure
applyCS :: ((Struct, Struct), (Struct, Struct)) -> Struct
applyCS ((r, q), (ua, Disj [])) = clean (Disj [Conj [Disj [r, ua], q], ua])
applyCS ((r, q), (ua, ub)) = clean (Disj [Conj [Disj [r, ua], q], ua, ub])


splitConjDisj :: Struct -> [(Struct, Struct)]
splitConjDisj (Disj b) = [(a, Disj (delete a b)) | a <- findAllConj (Disj b)]
  where
    findAllConj (Disj xs) = [x | x <- xs, isConj x]
    isConj (Conj _) = True
    isConj _ = False
splitConjDisj _ = []

-- Finds all permutations
findPermutations :: Struct -> [Struct]
findPermutations (Disj a) = [Disj px | px <- permutations a]
findPermutations (Conj a) = [Conj px | px <- permutations a]

-- Finds all partitions
findPartitions :: Struct -> [(Struct, Struct)]
findPartitions (Disj a) = [(Disj (take i a), Disj (drop i a)) | i <- [1..(length a - 1)]]
findPartitions (Conj a) = [(Conj (take i a), Conj (drop i a)) | i <- [1..(length a - 1)]]


-- Permutes and partitions a structure
combinePermutePartition :: (Struct, Struct) -> [((Struct, Struct), (Struct, Struct))]
combinePermutePartition (a, b) = combine (permuteAndPartition a)(permuteAndPartition b)
    where
        combine a b = [(x, y) | x <- a, y <- b]
        permuteAndPartition a = concatMap findPartitions (findPermutations a)


-- Finds all combinations
findAllCombinations :: Struct -> [((Struct, Struct), (Struct, Struct))]
findAllCombinations a = concatMap combinePermutePartition (splitConjDisj a)





-- Filters improper conjunctions and disjunctions
filterProper :: [((Struct, Struct), (Struct, Struct))] -> [((Struct, Struct), (Struct, Struct))]
filterProper = filter notProper
  where
    notProper ((r, q), (ua, ub)) = not (isDisj ua) && not (isConj r)

-- Filters based on legal intersections for conjunction-disjunction switching
filterLegalIntersection :: [((Struct, Struct), (Struct, Struct))] -> [((Struct, Struct), (Struct, Struct))]
filterLegalIntersection = filter legalIntersection
  where
    legalIntersection ((r, q), (ua, ub)) = intersect (negAll (allAtoms r)) (allAtoms ua) /= []

-- Finds legal combinations of partitions for CS rules
findLegalCS :: Struct -> [((Struct, Struct), (Struct, Struct))]
findLegalCS a = filterLegalIntersection (filterProper (findAllCombinations (clean a)))

-- Applies CS rules to a structure (simplified version)
applyCS :: ((Struct, Struct), (Struct, Struct)) -> Struct
applyCS ((r, q), (ua, Disj [])) = clean (Conj [Disj [r, ua], q])  -- Simplified case with no extra disjunction
applyCS ((r, q), (ua, ub)) = clean (Conj [Disj [r, ua], ub])  -- Removed the additional disjunction terms

-- Applies CS rules to a structure
applyCS :: ((Struct, Struct), (Struct, Struct)) -> Struct
applyCS ((r, q), (ua, Disj [])) = clean (Disj [Conj [Disj [r, ua], q], ua])
applyCS ((r, q), (ua, ub)) = clean (Disj [Conj [Disj [r, ua], q], ua, ub])


-- Finds and applies the deepest CS rules
applyDeepestCS :: ((Struct, Struct), (Struct, Struct)) -> [Struct]
applyDeepestCS ((r, q), (ua, ub))
  | findLegalCS (clean r) /= [] = [clean (Disj [Conj [r1, q], ua, ub]) | r1 <- cs r]
  | findLegalCS (clean ua) /= [] = [clean (Disj [Conj [r, q], ua1, ub]) | ua1 <- cs ua]
  | otherwise = [applyCS ((r, q), (ua, ub))]


-- Applies CS to all permutations
cs :: Struct -> [Struct]
cs (Disj a)
  | null (findLegalCS (Disj a)) = [Disj a]
  | otherwise = concatMap applyDeepestCS (findLegalCS (Disj a))
cs a = [a]

-- Lists all CS structures for a given structure
csList :: Struct -> [Struct]
csList (Disj a) = concatMap applyDeepestCS (findLegalCS (Disj a))
csList _ = []

-- Applies depth-first search with a given function
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
treeSize _ = 1

-- Recurses down to a number and returns only that substructure
dfsToNumberNonRecursive :: Struct -> Int -> Struct
dfsToNumberNonRecursive x i = (dfs2 x i) !! 0
  where
    dfs2 x 0 = [x]
    dfs2 (Disj x) i = concat [dfs2 ab | (a, b) <- zip x (computeDFSNumbers i x)]
    dfs2 (Conj x) i = concat [dfs2 ab | (a, b) <- zip x (computeDFSNumbers i x)]
    dfs2 a i = []


-- List all CS structures
listAll :: Struct -> (Struct -> Struct) -> [Struct]
listAll x f = delete x (nub (map (dfsToNumber f x) [0..((treeSize x)-1)]))
listAllCS :: Struct -> [Struct]
listAllCS x = map clean (delete x (nub (concatMap (listCSforSubStruct x) [0..((treeSize x) - 1)])))
listCSforSubStruct :: Struct -> Int -> [Struct]
listCSforSubStruct x i = nub [dfsToNumber (\y -> ls !! j) x i | j <- [0..(length ls) - 1]]
  where
    ls = csList (dfsToNumberNonRecursive x i)


-- Test cases for 'w2' function
w2Test1 = TestCase (assertEqual "w2 Test 1:" T (w2 (parseStruct "[T v a v b v −a v F]")))
w2Test2 = TestCase (assertEqual "w2 Test 2:" (parseStruct "[b v c]") (w2 (parseStruct "[b v c]")))
w2Test3 = TestCase (assertEqual "w2 Test 3:" (parseStruct "(a^−a^T)") (w2 (parseStruct "(a^−a^T)")))
w2Test4 = TestCase (assertEqual "w2 Test 4:" T (w2 (parseStruct "[a v b v c]")))

w2Tests = TestList [ TestLabel "w2Test1" w2Test1
                   , TestLabel "w2Test2" w2Test2
                   , TestLabel "w2Test3" w2Test3
                   , TestLabel "w2Test4" w2Test4 ]


-- Filters improper conjunctions and disjunctions
filterProper :: [((Struct, Struct), Struct)] -> [((Struct, Struct), Struct)]
filterProper = filter proper
  where
    proper ((r, q), u) = (isDisj u) || (isConj r)

-- Filters based on legal intersections for conjunction-disjunction switching
filterLegalIntersection :: [((Struct, Struct), Struct)] -> [((Struct, Struct), Struct)]
filterLegalIntersection = filter legalIntersection
  where
    legalIntersection ((r, q), u) = intersect (negAll (allAtoms r)) (allAtoms u) /= []

-- Finds legal combinations of partitions for switch rules
findLegalSwitch :: Struct -> [((Struct, Struct), Struct)]
findLegalSwitch a = filterLegalIntersection (filterProper (findAllCombinations (clean a)))


-- Checks if a structure is a proper disjunction of conjunction
isDisj :: Struct -> Bool
isDisj (Disj a) = countUnit (Disj a) >= 2
isDisj _        = False

isConj :: Struct -> Bool
isConj (Conj a) = countUnit (Conj a) >= 2
isConj _        = False


acDown :: Struct -> [Struct]
acDown (Disj a) =
    let atoms = openAtoms a
        results = map clean $ concatMap (modifySingleElement a) a
    in nub results
    where 
      openAtoms a = [x | x <- a, isAtom x]
acDown (Conj a) =
    let results = map clean $ concatMap (\x -> map (replaceElement a x) (acDown x)) a
    in results
acDown (Atom a) = [Disj [Atom a, Atom a]]
acDown (Neg atom) = [Disj [Neg atom, Neg atom]]
acDown a = [a]

dfsToNumberNonRecursive :: Struct -> Int -> Struct
dfsToNumberNonRecursive x i = 
  let result = dfs2 x i 
  in if null result then error "dfs2 returned an empty list" else result !! 0
  where
    dfs2 :: Struct -> Int -> [Struct]
    dfs2 x 0 = [x]
    dfs2 (Disj xs) i 
      | i >= length xs = [x]
      | null xs = [x]
      | otherwise = concat [dfs2 x (i - 1) | x <- xs]
    dfs2 (Conj xs) i 
      | i >= length xs = [x]
      | null xs = [x]
      | otherwise = concat [dfs2 x (i - 1) | x <- xs]
    dfs2 x i = [x]


    --- *** Inference Rules *** ---

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


acUp :: Struct -> Struct
acUp (Conj a) = toSingleton (Conj (nubBy equalAtom (map acUp a))) 
acUp a = a 


{-
acDown :: Struct -> Struct
acDown (Atom "a") = Disj [Atom "a", Atom "a"]
acDown (Neg (Atom "a")) = Disj [Neg (Atom "a"), Neg (Atom "a")]
acDown a = a


awDown :: Struct -> Struct
awDown (Atom "a") = F
awDown (Neg (Atom ㄏ"a")) = F
awDown a = a

awfDown :: Struct -> Struct
awfDown (Disj a) = 
-}


acDown :: Struct -> [Struct]
acDown (Disj a) =
    let results = map clean $ concatMap (modifySingleElement a) a
    in nub results
acDown (Conj a) =
    let results = map clean $ concatMap (\x -> map (replaceElement a x) (acDown x)) a
    in nub results
acDown (Atom a) = [Disj [Atom a, Atom a]]
acDown (Neg atom) = [Disj [Neg atom, Neg atom]]
acDown a = [a]

replaceElement :: [Struct] -> Struct -> Struct -> Struct
replaceElement a old new = Conj (map (\x -> if x == old then new else x) a)

modifySingleElement :: [Struct] -> Struct -> [Struct]
modifySingleElement a elemToModify =
    let modifiedElems = acDown elemToModify 
    in [Disj (map (\e -> if e == elemToModify then newElem else e) a) | newElem <- modifiedElems]


awfDown :: Struct -> [Struct]
awfDown (Disj a) =
  let atoms = openAtoms a
      uniqueAtoms = nubBy equalAtom atoms
      results = [Disj (delete atom a) | atom <- uniqueAtoms]
  in results
  where openAtoms a = [x | x <- a, isAtom x]
awfDown (Conj a) =
  let results = sequence (map awfDown a)  
  in map Conj results 
awfDown a = [a]



fDown :: Struct -> Struct
fDown (Disj a) = toSingleton (Disj (filter (/= F) a))
fDown a = a

tDown :: Struct -> Struct
tDown (Conj a) = toSingleton (Conj (filter (/= T) a))
tDown a = a


---
awfDown :: Struct -> [Struct]
awfDown (Disj a) =
  let atoms = openAtoms a
      uniqueAtoms = nubBy equalAtom atoms
      results = [Disj (delete atom a) | atom <- uniqueAtoms]
  in results
  where openAtoms a = [x | x <- a, isAtom x]
awfDown (Conj a) =
  let results = sequence (map awfDown a)  
  in map Conj results 
awfDown a = [a]
----

preferAIJustCS :: ProofTree -> ProofTree -> ProofTree
preferAIJustCS t = proofStep (listToTree . pickFirstCSAi . (removeRepeats t). sWithTag)
pickFirstCSAi :: [(a, Struct)] -> [(a, Struct)]
pickFirstCSAi xs
  | null xs = []
  | not (null [ x | x <- xs, not (null (listAll (snd x) aiWeakDown ))]) = [[x | x <- xs, not (null (listAll (snd x) aiWeakDown))] !! 0]
  | otherwise = [xs !! 0]

speedyAI :: ProofTree -> ProofTree
speedyAI t
  | nonCSStep t t /= t = nonCSStep t t
  | otherwise          = preferAIJustCS t t


nonCSStep :: ProofTree -> ProofTree -> ProofTree
nonCSStep t (ProofTree r s [End]) = ProofTree r s [End]
nonCSStep t (ProofTree r s [Empty])
  | removeRepeats t (listNonSwitchMedialRules s) == [] = ProofTree r s [Empty]
  | otherwise = ProofTree r s (listToTree (removeRepeats t (listNonSwitchMedialRules s)))
nonCSStep t (ProofTree r s xs) = ProofTree r s (map (nonCSStep t) xs)


nonCSStepDFS :: ProofTree -> ProofTree -> ProofTree
nonCSStepDFS t (ProofTree r s [End]) = ProofTree r s [End]
nonCSStepDFS t (ProofTree r s [Empty]) 
  | removeRepeats t (listNonSwitchMedialRules s) == [] = ProofTree r s [Empty]
  | otherwise = ProofTree r s  (listToTree [(removeRepeats t (listNonSwitchMedialRules s))!!0] )
nonCSStepDFS t (ProofTree r s xs)  = ProofTree r s (map (nonCSStepDFS t) xs)


onlyCSStep :: ProofTree -> ProofTree -> ProofTree
onlyCSStep t (ProofTree r s [End]) = ProofTree r s [End]
onlyCSStep t (ProofTree r s [Empty])
  | null (removeRepeats t ([( "s" , x) | x <- switchList s])) = ProofTree r s [Empty]
  | otherwise = ProofTree r s (listToTree (removeRepeats t ([("s",x) | x <- switchList s ])))
onlyCSStep t (ProofTree r s xs) = ProofTree r s (map (onlyCSStep t) xs)
 
speedyNotCS :: ProofTree -> ProofTree
speedyNotCS t
  | nonCSStep t t /= t = nonCSStep t t
  | otherwise          = onlyCSStep t t

speedyDFS :: ProofTree -> ProofTree
speedyDFS t
  | nonCSStepDFS t t /= t = nonCSStepDFS t t
  | otherwise             = csLastStep t t


csLastStep :: ProofTree -> ProofTree -> ProofTree
csLastStep t (ProofTree r s [End]) = ProofTree r s [End]
csLastStep t (ProofTree r s [Empty])
  | null (listAllRules s)                         = ProofTree r s []
  | null (removeRepeats t (listAllRules s))       = ProofTree r s [End]
  | not (null (removeRepeats t (listNonSwitchMedialRules s))) = ProofTree r s (listToTree (removeRepeats t (listNonSwitchMedialRules s)))
  | otherwise                                     = ProofTree r s (listToTree (removeRepeats t ([("s", r) | r <- (switchList s)])))
csLastStep t (ProofTree r s xs)  = ProofTree r s (map (csLastStep t) xs)


csBFSSingle t = csBFS t t
csBFS :: ProofTree -> ProofTree -> ProofTree
csBFS t (ProofTree r s [End]) = ProofTree r s [End]
csBFS t (ProofTree r s [Empty])
  | null (listAllRules s)                         = ProofTree r s []
  | null (removeRepeats t (listAllRules s))       = ProofTree r s [End]
  | not (null (removeRepeats t (listNonSwitchMedialRules s))) = ProofTree r s [(listToTree (removeRepeats t (listNonSwitchMedialRules s))) !! 0]
  | otherwise                                     = ProofTree r s (listToTree (removeRepeats t ([("s", r) | r <- (switchList s)])))
csBFS t (ProofTree r s xs) = ProofTree r s (map (csBFS t) xs)


csDFSSingle t = csDFS t t
csDFS :: ProofTree -> ProofTree -> ProofTree
csDFS t (ProofTree r s [End]) = ProofTree r s [End]
csDFS t (ProofTree r s [Empty])
  | null (listAllRules s)                         = ProofTree r s []
  | null (removeRepeats t (listAllRules s))       = ProofTree r s [End]
  | not (null (removeRepeats t (listNonSwitchMedialRules s))) = ProofTree r s (listToTree (removeRepeats t (listNonSwitchMedialRules s)))
  | otherwise                                     = ProofTree r s [(listToTree (removeRepeats t ([("s", r) | r <- (switchList s)]))) !! 0]
csDFS t (ProofTree r s xs) = ProofTree r s (map (csDFS t) xs)




noRepeatStepSingle :: ProofTree -> ProofTree
noRepeatStepSingle t = noRepeatStep t t
noRepeatStep :: ProofTree -> ProofTree -> ProofTree
noRepeatStep t (ProofTree r s [End]) = ProofTree r s [End]
noRepeatStep t (ProofTree r s [Empty])
  | listAllRules s == [] = ProofTree r s []
  | removeRepeats t (listNonSwitchMedialRules s) == [] = ProofTree r s [End]
  | otherwise = ProofTree r s (listToTree (removeRepeats t (listAllRules s)))
noRepeatStep t (ProofTree r s xs) = ProofTree r s (map (noRepeatStep t) xs)

sWithTag x = [("s",r) | r <- map clean (listAllSwitch x)]
mWithTag x = [("m",r) | r <- map clean (listAllMedial x)]




searchForProof2 :: (ProofTree -> ProofTree -> ProofTree) -> ProofTree -> [(String, Struct)]
searchForProof2 f x
  | not (null (pathsToTrue x)) = pathsToTrue x !! 0
  | not (null (pathsToDone x)) = pathsToDone x !! 0
  | otherwise                  = searchForProof2 f (f x x)

  -- 主函數
main :: IO ()
main = do
  examples <- generateExamples 200 -- 每次生成不同的 200 個隨機結構
  defaultMain [
    bgroup "BFS vs DFS" (generateBenchmarks examples)
    ]

main :: IO ()
main = do
  examples <- generateExamples 200
  print examples 

  defaultMain [
    bgroup "structure-combinations" [
        bench "test-s1" $ nf (length . show) s1,
        bench "test-s2" $ nf (length . show) s2,
        bench "test-pc1" $ nf (length . show) pc1,
        bench "test-pc2" $ nf (length . show) pc2
    ]
   ]



-- 包裝搜索函數並加上超時限制
timeoutBenchmark :: String -> (Struct -> [(String, Struct)]) -> Struct -> Benchmark
timeoutBenchmark name searchFunc struct = 
    bench name $ nfIO $ timeout timeoutLimit (evaluate (rnf (searchFunc (startTree struct))))

-- 生成測試組合
generateBenchmarks :: [Struct] -> [Benchmark]
generateBenchmarks examples = 
    [ bgroup ("Test example " ++ show i) 
        [ timeoutBenchmark "BFS" searchForProof bfsProofStep (examples !! i),
          timeoutBenchmark "DFS" searchForProof dfsProofStep (examples !! i),
          timeoutBenchmark "BFS Strict" searchForProof bfsStrict (examples !! i),
          timeoutBenchmark "DFS Strict" searchForProof dfsStrict (examples !! i),
          timeoutBenchmark "BFS NoRepeat" searchForProof bfsStrictNoRepeat (examples !! i),
          timeoutBenchmark "DFS NoRepeat" searchForProof dfsStrictNoRepeat (examples !! i),
          timeoutBenchmark "DFS Backtrack" searchForProof dfsProofStepWithBacktrack (examples !! i)
        ] | i <- [0..99]
    ]


--generateBenchmarks :: [Struct] -> [Benchmark]
generateBenchmarks examples = 
    [ bgroup ("Comparison between different proof search methods " ++ show i) 
        [ timeoutBenchmark "BFS" testFunction bfsProofStep (examples !! i),
          timeoutBenchmark "DFS" testFunction dfsProofStep (examples !! i),
          timeoutBenchmark "BFS Strict" testFunction bfsStrict (examples !! i),
          timeoutBenchmark "DFS Strict" testFunction dfsStrict (examples !! i),
          timeoutBenchmark "BFS NoRepeat" testFunction bfsStrictNoRepeat (examples !! i),
          timeoutBenchmark "DFS NoRepeat" testFunction dfsStrictNoRepeat (examples !! i),
          timeoutBenchmark "DFS Backtrack" testFunction dfsProofStepWithBacktrack (examples !! i)
        ] | i <- [0..99]
    ]

timeoutBenchmark :: String -> (Struct -> [(String, Struct)]) -> Struct -> Benchmark
timeoutBenchmark name searchFunc struct = 
    bench name $ nfIO $ timeout timeoutLimit (evaluate (rnf (searchFunc (startTree ((clean . removeImpEqu) struct)))))