{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Alphabets 
  ( Password ()
  , VLine ()
  , VTable ()
  , Clear ()
  , Hidden ()
  , mkPassword
  , mkCleartext
  , mkHiddentext
  , vigengL
  , gnegivL
  , passlist
  , passmatrix
  , canonicallist
  , samplePass
  , sampleMatrix
  , rescape
  ) where

import Canonical (Canonical (..), Heretical (readHeresies))
import Data.Maybe (fromJust)
import Data.List (elemIndex)



rescape :: [Char] -> [Char]
rescape = map (\case '~' -> '\n'; '_' -> ' '; x -> x)

newtype VLine = VLine [Canonical]

instance Show VLine where
  show :: VLine -> String
  show (VLine xs) =
      "***ONE VLine***\n" ++ 
      filter (/= '\n') (show xs)

newtype VTable = VTable [[Canonical]]

instance Show VTable where
  show :: VTable -> String
  show (VTable xs) = 
     "***START VTable***\n" ++  
      filter (/= '\n') (show (concat xs)) ++ 
     "\n***END VTable***"

newtype Password = Password [Canonical]

instance Show Password where
  show :: Password -> String
  show _ = "***"

mkPassword :: Heretical a => [a] -> Password
mkPassword x
    | notElem Nothing (readHeresies x) =
        Password $ map fromJust $ readHeresies x
    | otherwise = error "mkPassword: Illegal Character in String"

newtype Hidden = Hidden VLine

mkHiddentext :: Heretical a => [a] -> Hidden
mkHiddentext x
    | notElem Nothing (readHeresies x) =
        Hidden $ VLine $ map fromJust $ readHeresies x
    | otherwise = error "mkHiddentext: Illegal Character in String"

instance Show Hidden where
  show :: Hidden -> String
  show (Hidden (VLine [])  )                  =   ""
  show (Hidden (VLine (x: xs))) =
      "***START Hidden***\n" ++ 
      show [x] ++ show xs ++ 
      "\n***END Hidden***"

newtype Clear = Clear VLine

mkCleartext :: Heretical a => [a] -> Clear
mkCleartext x
    | notElem Nothing (readHeresies x) =
        Clear $ VLine $ map fromJust $ readHeresies x
    | otherwise = error "mkCleartext: Illegal Character in String"

instance Show Clear where
  show :: Clear -> String
  show (Clear (VLine [])       ) = ""
  show (Clear (VLine (x: xs))) =
      "***START Clear***\n" ++ 
      (show [x] ++ show xs) ++ 
      "\n***END Clear***"

canonicallist :: VLine
canonicallist = VLine [minBound .. maxBound]

samplePass :: Password
samplePass = Password [LL, LA, LB, LR, LA, LD, LO, LR]

passlist :: VLine -> Password -> VLine
passlist list (Password []) = list
passlist (VLine (x:xs)) (Password (p:assword)) =
    passlist (VLine (p: filter (/=p) (x:xs))) (Password assword)
passlist _ _ = VLine []


passmatrix :: VLine -> VTable
passmatrix (VLine a) =
    VTable $ passmatrix' [] a (length a)
  where
    passmatrix' acc _ 0 = acc
    passmatrix' acc (x : xs) i =
        passmatrix' (acc ++ [x : xs]) (xs ++ [x]) (i - 1)
    passmatrix' _ _ _ = []

sampleMatrix :: VTable
sampleMatrix = passmatrix canonicallist

vigengL :: VTable -> Clear -> Password -> Hidden
vigengL (VTable a) (Clear (VLine b)) (Password c) =
    Hidden $ VLine $ vigengL' a c b c
  where
    vigengL' :: [[Canonical]] -> [Canonical] ->[Canonical] -> [Canonical] -> [Canonical]
    vigengL' _ [] _ _ = []
    vigengL' _ _ [] _ = []
    vigengL' matrix (x:xs) (y:ys) p =
        vigeng matrix x y : vigengL' matrix (xs++[x]) ys p
    vigeng :: [[Canonical]] -> Canonical ->Canonical -> Canonical
    vigeng matrix pass input =
        head $ drop j $ head $ drop i matrix
      where
        i = fromEnum pass
        j = fromEnum input

gnegivL :: VTable -> Hidden -> Password -> Clear
gnegivL (VTable a) (Hidden (VLine b)) (Password c) =
    Clear $ VLine $ gnegivL' a c b c
  where
    gnegivL' :: [[Canonical]] -> [Canonical] ->[Canonical] -> [Canonical] -> [Canonical]
    gnegivL' _ [] _ _ = []
    gnegivL' _ _ [] _ = []
    gnegivL' matrix (x:xs) (y:ys) p =
        gnegiv matrix x y : gnegivL' matrix (xs++[x]) ys p
    gnegiv :: [[Canonical]] -> Canonical ->Canonical -> Canonical
    gnegiv matrix pass input =
        toEnum $ fromJust j
      where
        i = fromEnum pass
        j = elemIndex input $ head $ drop i matrix