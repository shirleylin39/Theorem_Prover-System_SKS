import Test.HUnit -- Must have HUnit installed to run these tests.
import Data.List
import Structures
import Rules
import Parser

-- runTestTT allTests
allTests = TestList ( concatTests [ aiDownTests , aiWeakDownTests , acUpTests , acDownTests , fDownTests, tDownTests, switchTests, medialTests , commutativityTests , associativityTests , deMorganTests ])
concatTests [] = []
concatTests ( TestList tests : ts ) = tests ++ concatTests ts

-- Test cases for 'aiDown' function
aiDownTest1 = TestCase (assertEqual "aiDown Test 1:" T (aiDown (parseStruct "[a v -a]")))
aiDownTest2 = TestCase (assertEqual "aiDown Test 2:" (parseStruct "[b v c]") (aiDown (parseStruct "[b v c]")))
aiDownTest3 = TestCase (assertEqual "aiDown Test 3:" (parseStruct "(a ^ -a)") (aiDown (parseStruct "(a ^ -a)")))
aiDownTest4 = TestCase (assertEqual "aiDown Test 4:" (parseStruct "[T v F v a v (b ^ c) v T]") (aiDown (parseStruct "[T v a v F v a v (b^c) v -a]")))
aiDownTest5 = TestCase (assertEqual "aiDown Test 5:" (parseStruct "[b v c v T]") (aiDown (parseStruct "[a v -a v b v c]")))
aiDownTest6 = TestCase (assertEqual "aiDown Test 6:" (parseStruct "[b v -b v c v T]") (aiDown (parseStruct "[a v -a v b v -b v c]")))
aiDownTests = TestList [ TestLabel "aiDownTest1" aiDownTest1, TestLabel "aiDownTest2" aiDownTest2, TestLabel "aiDownTest3" aiDownTest3, TestLabel "aiDownTest4" aiDownTest4, TestLabel "aiDownTest5" aiDownTest5, TestLabel "aiDownTest6" aiDownTest6 ]

-- Test cases for 'aiWeakDown' function
aiWeakDownTest1 = TestCase (assertEqual "aiWeakDown Test 1:" T (aiWeakDown (parseStruct "[a v -a]")))
aiWeakDownTest2 = TestCase (assertEqual "aiWeakDown Test 2:" (parseStruct "[b v c]") (aiWeakDown (parseStruct "[b v c]")))
aiWeakDownTest3 = TestCase (assertEqual "aiWeakDown Test 3:" (parseStruct "(a ^ -a)") (aiWeakDown (parseStruct "(a ^ -a)")))
aiWeakDownTest4 = TestCase (assertEqual "aiWeakDown Test 4:" T (aiWeakDown (parseStruct "[T v a v F v a v (b ^ c) v -a]")))
aiWeakDownTest5 = TestCase (assertEqual "aiWeakDown Test 5:" T (aiWeakDown (parseStruct "[a v -a v b v c]")))
aiWeakDownTest6 = TestCase (assertEqual "aiWeakDown Test 6:" T (aiWeakDown (parseStruct "[a v -a v b v -b v c]")))
aiWeakDownTests = TestList [ TestLabel "aiWeakDownTest1" aiWeakDownTest1, TestLabel "aiWeakDownTest2" aiWeakDownTest2, TestLabel "aiWeakDownTest3" aiWeakDownTest3, TestLabel "aiWeakDownTest4" aiWeakDownTest4, TestLabel "aiWeakDownTest5" aiWeakDownTest5, TestLabel "aiWeakDownTest6" aiWeakDownTest6 ]
                           
-- Test cases for 'acUp' function
acUpTest1 = TestCase (assertEqual "acUp Test 1:" (parseStruct "a") (acUp (parseStruct "(a ^ a)")))
acUpTest2 = TestCase (assertEqual "acUp Test 2:" (parseStruct "[b v c]") (acUp (parseStruct "[b v c]")))
acUpTest3 = TestCase (assertEqual "acUp Test 3:" (parseStruct "(a ^ -a ^ T ^ c ^ b)") (acUp (parseStruct "(a ^ -a ^ T ^ T ^ c ^ b ^ c ^ c)")))
acUpTest4 = TestCase (assertEqual "acUp Test 4:" (parseStruct "a") (acUp (parseStruct"(a ^ a ^ a ^ a ^ a)")))
acUpTests = TestList [ TestLabel "acUpTest1" acUpTest1, TestLabel "acUpTest2" acUpTest2, TestLabel "acUpTest3" acUpTest3, TestLabel "acUpTest4" acUpTest4 ]

-- Test cases for 'acDown' function
acDownTest1 = TestCase (assertEqual "acDown Test 1:" (parseStruct "[a v a]") (acDown (parseStruct "a")))
acDownTest2 = TestCase (assertEqual "acDown Test 2:" (parseStruct "[-a v -a]") (acDown (parseStruct"-a")))
acDownTests = TestList [ TestLabel "acDownTest1" acDownTest1, TestLabel "acDownTest2" acDownTest2 ]

-- Test cases for 'awDown' function
awDownTest1 = TestCase (assertEqual "awDown Test 1:" F (awDown (parseStruct "a")))
awDownTest2 = TestCase (assertEqual "awDown Test 2:" F (awDown (parseStruct "-a")))
awDownTests = TestList [ TestLabel "awDownTest1" awDownTest1, TestLabel "awDownTest2" awDownTest2 ]
                       
-- Test cases for 'fDown' function
fDownTest1 = TestCase (assertEqual "fDown Test 1:" (parseStruct "[a v -a]") (fDown (parseStruct "[F v F v a v F v -a v F]")))
fDownTest2 = TestCase (assertEqual "fDown Test 2:" (parseStruct "[b v c]") (fDown (parseStruct "[b v c]")))
fDownTest3 = TestCase (assertEqual "fDown Test 3:" (parseStruct "(a ^ -a)") (fDown (parseStruct "(a ^ -a)")))
fDownTest4 = TestCase (assertEqual "fDown Test 4:" F (fDown (parseStruct "[F]")))
fDownTests = TestList [ TestLabel "fDwonTest1" fDownTest1, TestLabel "fDownTest2" fDownTest2, TestLabel "fDownTest3" fDownTest3, TestLabel "fDownTest4" fDownTest4 ]
                   
-- Test cases for 'tDown' function
tDownTest1 = TestCase (assertEqual "tDown Test 1:" (parseStruct "([a v -a] ^ F ^ c)") (tDown (parseStruct "(T ^ [a v -a] ^ T ^ F ^ c)")))
tDownTest2 = TestCase (assertEqual "tDown Test 2:" (parseStruct "[b v c]") (tDown (parseStruct "[b v c]")))
tDownTest3 = TestCase (assertEqual "tDown Test 3:" (parseStruct "(a ^ -a)") (tDown (parseStruct "(a ^ -a ^ T)")))
tDownTest4 = TestCase (assertEqual "tDown Test 4:" T (tDown (parseStruct "(T)")))

tDownTests = TestList [ TestLabel "tDownTest1" tDownTest1, TestLabel "tDownTest2" tDownTest2, TestLabel "tDownTest3" tDownTest3, TestLabel "tDownTest4" tDownTest4 ]
    
-- Test cases for 'switch' function
switchTest1 = TestCase (assertEqual "switch Test 1:" [parseStruct "a"] (switch (parseStruct "a")))
switchTest2 = TestCase (assertEqual "switch Test 2:" [parseStruct "([a v -a] ^ b)"] (switch (parseStruct "[(a ^ b) v -a]")))
switchTest3 = TestCase (assertEqual "switch Test 3:" [parseStruct "[([a v -a] ^ b) v d v (-d ^ b)]", parseStruct "[([-d v d] ^ b) v -a v (a ^ b)]"] (nubBy equalComm (switch (parseStruct "[(a^b) v -a v d v (-d^b)]"))))
switchTest4 = TestCase (assertEqual "switch Test 4:" [T] (switch (parseStruct "T")))
switchTests = TestList [ TestLabel "switchTest1" switchTest1, TestLabel "switchTest2" switchTest2, TestLabel "switchTest3" switchTest3, TestLabel "switchTest4" switchTest4 ]
                   
-- Test cases for 'medial' function
medialTest1 = TestCase (assertEqual "Medial Test 1:" [parseStruct "[(a ^ a) v (b ^ d)]"](nubBy equalComm (medial (parseStruct "([a v b] ^ [a v d])"))))
medialTest2 = TestCase (assertEqual "Medial Test 2:" [parseStruct "[(c ^ c) v ([b v a] ^ d)]"](nubBy equalComm (medial (parseStruct "([a v b v c] ^ [c v d])"))))           
medialTest3 = TestCase (assertEqual "Medial Test 3:" [parseStruct "([(b ^ b) v (a ^ c)] ^ [c v d])", parseStruct "([(c ^ c) v (b ^ d)] ^ [a v b])"] (nubBy equalComm (medial (parseStruct "([a v b] ^ [b v c] ^ [c v d])"))))
medialTest4 = TestCase (assertEqual "Medial Test 4:" [parseStruct "([(b ^ b) v ([a v c] ^ c)] ^ x)", parseStruct "([(c ^ c) v ([b v a] ^ b)] ^ x)"](nubBy equalComm (medial (parseStruct "([a v b v c] ^ [b v c] ^ x)"))))
medialTests = TestList [ TestLabel "Medial Test 1" medialTest1, TestLabel "Medial Test 2" medialTest2 , TestLabel "Medial Test 3" medialTest3, TestLabel "Medial Test 4" medialTest4 ]

-- Test cases for commutativity
commutativityTest1 = TestCase (assertEqual "commutativity Test 1:" True (equalComm (parseStruct "[a v b]") (parseStruct "[b v a]")))
commutativityTest2 = TestCase (assertEqual "commutativity Test 2:" True (equalComm (parseStruct "(a ^ b)") (parseStruct "(b ^ a)")))
commutativityTest3 = TestCase (assertEqual "commutativity Test 3:" True (equalComm (parseStruct "(a ^ b ^ [c v (a ^ -d)]^T)") (parseStruct "(T ^ b ^ [(-d ^ a) v c] ^ a)")))
commutativityTest4 = TestCase (assertEqual "commutativity Test 4:" False (equalComm (parseStruct "[a v b v c]") (parseStruct "[b v a]")))
commutativityTests = TestList [ TestLabel "commutativityTest1" commutativityTest1, TestLabel "commutativityTest2" commutativityTest2, TestLabel "commutativityTest3" commutativityTest3, TestLabel "commutativityTest4" commutativityTest4 ]

-- Test cases for associativity
associativityTest1 = TestCase (assertEqual "associativity Test 1:" (parseStruct "[a v b v c]") (assoc (parseStruct "[a v [b v c]]")))
associativityTest2 = TestCase (assertEqual "associativity Test 2:" (parseStruct "(a ^ b ^ c)") (assoc (parseStruct "(a ^ (b ^ c))")))
associativityTest3 = TestCase (assertEqual "associativity Test 3:" (parseStruct "[a v (a ^ c ^ b) v d]") (assoc (parseStruct "[[a v (a ^ (c ^ (b)))] v [d]]")))
associativityTest4 = TestCase (assertEqual "associativity Test 4:" T (assoc (parseStruct "T")))
associativityTests = TestList [ TestLabel "associativityTest1" associativityTest1, TestLabel "associativityTest2" associativityTest2, TestLabel "associativityTest3" associativityTest3, TestLabel "associativityTest4" associativityTest4 ]
                              
-- Test cases for de Morgan
deMorganTest1 = TestCase (assertEqual "de Morgan Test 1:" (parseStruct "[-a v -b]") (deMorgan (parseStruct "- {(a ^ b)}")))
deMorganTest2 = TestCase (assertEqual "de Morgan Test 2:" (parseStruct "(-a ^ -b)") (deMorgan (parseStruct "- {[a v b]}")))
deMorganTest3 = TestCase (assertEqual "de Morgan Test 3:" (parseStruct "(a ^ -a)") (deMorgan (parseStruct "(a ^ -a)")))
deMorganTest4 = TestCase (assertEqual "de Morgan Test 4:" (parseStruct "[-a v (-b ^ c) v (a ^ -a)]") (deMorgan (parseStruct "-{(a ^ [b v -c] ^ -{(a ^ -a)})}")))
deMorganTests = TestList [ TestLabel "deMorganTest1" deMorganTest1, TestLabel "deMorganTest2" deMorganTest2, TestLabel "deMorganTest3" deMorganTest3, TestLabel "deMorganTest4" deMorganTest4 ]
                        
-- To run all tests
main :: IO ()
main = runTestTT allTests >>= print