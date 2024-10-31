{-# LANGUAGE InstanceSigs #-}
module Alphabets where

import Canonical (Canonical (..))
import Data.Maybe (fromJust)
import Data.List (elemIndex)

newtype VLine = VLine [Canonical]

instance Show VLine where
  show :: VLine -> String
  show (VLine x) = show x

newtype VTable = VTable [[Canonical]]

instance Show VTable where
  show :: VTable -> String
  show (VTable x) = show $ concat x

newtype Password = Password [Canonical]

newtype Hidden = Hidden VLine

instance Show Hidden where
  show :: Hidden -> String
  show (Hidden (VLine [])  )                  =   ""
  show (Hidden (VLine (x: xs))) =  show [x] ++ show (Hidden $ VLine xs)
 
newtype Clear = Clear VLine

instance Show Clear where
  show :: Clear -> String
  show (Clear (VLine [])       )             =   ""
  show (Clear (VLine (x: xs))) =  show [x] ++ show (Clear $ VLine xs)

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

testmatrix :: VTable
testmatrix = passmatrix canonicallist

vigengL :: VTable -> Clear -> Password -> Hidden
vigengL (VTable a) (Clear (VLine b)) (Password c) =  
    Hidden $ VLine $ vigengL' a c b c 
  where
    vigengL' _ [] _ _ = []
    vigengL' _ _ [] _ = []
    vigengL' matrix (x:xs) (y:ys) p =
        vigeng matrix x y : vigengL' matrix (xs++[x]) ys p
    
    vigeng matrix pass input =
        head $ drop j $ head (drop i matrix)
      where
        i = fromEnum pass
        j = fromEnum input

gnegivL :: VTable -> Hidden -> Password -> Clear
gnegivL (VTable a) (Hidden (VLine b)) (Password c) =  
    Clear $ VLine $ gnegivL' a c b c
  where
    gnegivL' _ [] _ _ = []
    gnegivL' _ _ [] _ = []
    gnegivL' matrix (x:xs) (y:ys) p = 
        gnegiv matrix x y : gnegivL' matrix (xs++[x]) ys p
    gnegiv matrix pass input = 
        toEnum $ fromJust j
      where
        i = fromEnum pass
        j = elemIndex input $ head $ drop i matrix