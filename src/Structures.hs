{-# LANGUAGE DeriveGeneric #-}
module Structures where
import Data.List
import Control.DeepSeq
import GHC.Generics (Generic)

-- Basic Structures

data Struct
    = T
    | F
    | Atom     String
    | Disj     [Struct]
    | Conj     [Struct]
    | Neg      Struct
    | Imp      Struct     Struct
    | Equ      Struct     Struct
    deriving (Eq, Generic)

instance NFData Struct

-- Remove Implication and Equalities
removeImpEqu :: Struct -> Struct
removeImpEqu (Imp a b) = Disj [Neg (removeImpEqu a), removeImpEqu b]
removeImpEqu (Disj a)  = Disj [removeImpEqu x | x <- a]
removeImpEqu (Conj a)  = Conj [removeImpEqu x | x <- a]
removeImpEqu (Equ a b) = Disj [Conj [removeImpEqu a, removeImpEqu b], Conj [Neg (removeImpEqu a), Neg (removeImpEqu b)]]
removeImpEqu (Neg a)   = Neg (removeImpEqu a)
removeImpEqu x         = x

-- Lists all atoms in a given structure
allAtoms :: Struct -> [Struct]
allAtoms (Disj a)       = concat [allAtoms x | x <- a]
allAtoms (Conj a)       = concat [allAtoms x | x <- a]
allAtoms (Atom a)       = [Atom a]
allAtoms T              = []
allAtoms F              = []
allAtoms (Neg (Atom a)) = [Neg (Atom a)]
allAtoms _              = []

-- Atom Negation
negAtom :: Struct -> Struct
negAtom (Atom a)       = Neg (Atom a)
negAtom (Neg (Atom a)) = Atom a

-- Transform formula to negation normal form
negAll :: [Struct] -> [Struct]
negAll as = [negAtom a | a <- as]

-- Check if a structure is an atom
isAtom :: Struct -> Bool
isAtom (Atom _)        = True
isAtom (Neg (Atom _))  = True
isAtom _               = False

-- Element Count (without T and F)
countUnit :: Struct -> Int
countUnit (Disj a) = length [x | x <- a, x /= F, x /= T]
countUnit (Conj a) = length [x | x <- a, x /= F, x /= T]
countUnit _        = 0

-- Checks equality under modulo commutativity
equalComm :: Struct -> Struct -> Bool
equalComm (Conj a) (Conj b) = all (inComm b) a && (length a == length b)
equalComm (Disj a) (Disj b) = all (inComm b) a && (length a == length b)
equalComm (Atom a) (Atom b) = a == b
equalComm (Neg a)  (Neg b)  = equalComm a b
equalComm T        T        = True
equalComm F        F        = False
equalComm _        _        = False

inComm :: [Struct] -> Struct -> Bool
inComm bs a = foldr ((||) . equalComm a) False bs

-- Check if atoms are equal
equalAtom :: Struct -> Struct -> Bool
equalAtom (Atom a) (Atom b)               = a == b
equalAtom (Neg (Atom a)) (Neg (Atom b))   = a == b
equalAtom T T                             = True
equalAtom F F                             = True
equalAtom (Atom _) (Neg (Atom _))         = False
equalAtom (Neg (Atom _)) (Atom _)         = False
equalAtom _ _                             = False

-- Frees single elements in disjunction and conjunction
toSingleton :: Struct -> Struct
toSingleton (Disj [a]) = a
toSingleton (Disj [])  = F
toSingleton (Conj [a]) = a
toSingleton (Conj [])  = T
toSingleton a          = a

-- Transform structure to associativity normal forms
assoc :: Struct -> Struct
assoc (Disj a) = toSingleton (Disj (concat [g (assoc x) | x <- a]))
  where
    g (Disj a) = a
    g x        = [x]
assoc (Conj a) = toSingleton (Conj (concat [f (assoc x) | x <- a]))
  where
    f (Conj a) = a
    f x        = [x]
assoc a = a

-- De Morgan's laws
deMorgan :: Struct -> Struct
deMorgan (Neg (Disj a)) = Conj [deMorgan (Neg x) | x <- a]
deMorgan (Neg (Conj a)) = Disj [deMorgan (Neg x) | x <- a]
deMorgan (Disj a)       = Disj [deMorgan x | x <- a]
deMorgan (Conj a)       = Conj [deMorgan x | x <- a]
deMorgan (Neg T)        = F
deMorgan (Neg F)        = T
deMorgan (Neg (Neg a))  = deMorgan a
deMorgan a              = a

clean :: Struct -> Struct
clean a = toSingleton (assoc (deMorgan a))

-- Print 
instance Show Struct where
    show = showEquation
showEquation :: Struct -> String
showEquation T             = "T"
showEquation F             = "F"
showEquation (Atom a)      = a
showEquation (Disj a)      = "[" ++ intercalate " v " [showEquation x | x <- a] ++ "]"
showEquation (Conj a)      = "(" ++ intercalate " ^ " [showEquation x | x <- a] ++ ")"
showEquation (Neg (Atom a))= "-" ++ a
showEquation (Neg a)       = "-{" ++ showEquation a ++ "}"
showEquation (Imp a b)     = "{" ++ showEquation a ++ " => " ++ showEquation b ++ "}"
showEquation (Equ a b)      = "{" ++ showEquation a ++ " <=> " ++ showEquation b ++ "}"


