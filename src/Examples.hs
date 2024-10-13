module Examples where
import Structures
import Parser

-- Statman tautologies: https://people.bath.ac.uk/ag248/t/TLPCL.pdf

s1 = Disj [Conj [Neg (Atom "a1"), Neg (Atom "b1")], Atom "a1", Atom "b1"]
s2 = (sn 2)

sn :: Int -> Struct
sn n
    | n == 1 = s1
    | otherwise = Disj [Conj [Neg (Atom ("a" ++ show n)), Neg (Atom ("b" ++ show n))], f (n-1), Disj [Atom "a1", Atom "b1"]]
  where
    f 1 = Disj [Conj [ank n 1, bnk n 1]]
    f i = Disj [Conj [ank n i, bnk n i], f (i-1)]

ank :: Int -> Int -> Struct
ank n k
    | n == k = Neg (Atom ("a" ++ show k))
    | otherwise = Conj [Disj [Atom ("a" ++ show n), Atom ("b" ++ show n)], ank (n-1) k]

bnk :: Int -> Int -> Struct
bnk n k
    | n == k = Neg (Atom ("b" ++ show k))
    | otherwise = Conj [Disj [Atom ("a" ++ show n), Atom ("b" ++ show n)], bnk (n-1) k]

-- Tautologies from https://www.uky.edu/~look/CheatSheet.pdf

pc1 = Imp (Conj [Atom "A1", Atom "B1"]) (Atom "A1")
pc2 = Imp (Conj [Atom "A2", Atom "B2"]) (Atom "B2")
pc3 = Imp (Imp (Atom "A3") (Atom "B3")) (Imp (Imp (Atom "A3") (Atom "C3")) (Imp (Atom "A3") (Conj [Atom "B3", Atom "C3"])))
pc4 = Imp (Atom "A4") (Imp (Atom "B4") (Conj [Atom "A4", Atom "B4"]))
pc5 = Imp (Imp (Atom "A5") (Atom "B5")) (Imp (Imp (Atom "B5") (Atom "A5")) (Equ (Atom "A5") (Atom "B5")))
pc6 = Imp (Imp (Atom "A6") (Atom "B6")) (Imp (Imp (Atom "B6") (Atom "C6")) (Imp (Atom "A6") (Atom "C6")))
pc7 = Imp (Imp (Atom "A7") (Imp (Atom "B7") (Atom "C7"))) (Imp (Conj [Atom "A7", Atom "B7"]) (Atom "C7"))
pc8 = Imp (Imp (Atom "A8") (Atom "B8")) (Imp (Imp (Atom "B8") (Imp (Atom "C8") (Atom "D8"))) (Imp (Conj [Atom "A8", Atom "D8"]) (Atom "D8")))
pc9 = Imp (Atom "A9") (Disj [Atom "A9", Atom "B9"])
pc10 = Imp (Atom "B10") (Disj [Atom "A10", Atom "B10"])
pc11 = Imp (Imp (Atom "A11") (Atom "B11")) (Imp (Imp (Atom "C11") (Atom "B11")) (Imp (Disj [Atom "A11", Atom "C11"]) (Atom "B11")))
pc12 = Equ (Atom "A12") (Neg (Neg (Atom "A12")))
pc13 = Equ (Disj [Atom "A13", Atom "B13"]) (Neg (Conj [Neg (Atom "A13"), Neg (Atom "B13")]))
pc14 = Equ (Conj [Atom "A14", Atom "B14"]) (Neg (Disj [Neg (Atom "A14"), Neg (Atom "B14")]))
pc15 = Equ (Imp (Atom "A15") (Atom "B15")) (Imp (Neg (Atom "B15")) (Neg (Atom "A15")))
pc16 = Equ (Disj [Atom "A16", Atom "B16"]) (Disj [Atom "B16", Atom "A16"])
pc17 = Equ (Conj [Atom "A17", Atom "B17"]) (Conj [Atom "B17", Atom "A17"])
pc18 = Equ (Disj [Disj [Atom "A18", Atom "B18"], Atom "C18"]) (Disj [Atom "A18", Disj [Atom "B18", Atom "C18"]])
pc19 = Equ (Conj [Conj [Atom "A19", Atom "B19"], Atom "C19"]) (Conj [Atom "A19", Conj [Atom "B19", Atom "C19"]])
pc20 = Equ (Atom "A20") (Disj [Atom "A20", Atom "A20"])
pc21 = Equ (Atom "A21") (Conj [Atom "A21", Atom "A21"])


mye1 = Disj [F, F, Atom "E1", Neg (Atom "E1")]
mye2 = Conj [T, T, Disj [F, Atom "E3", Atom "E3", Atom "E2", Neg (Atom "E2")], Disj [Atom "E2", Neg (Atom "E2")]]


modusPonens = parseStruct "{(p ^ {p} => {q})} => {q}"
reductio = parseStruct "{{a} => {b}} <=> {{(a ^ -b)} => {F}}"


