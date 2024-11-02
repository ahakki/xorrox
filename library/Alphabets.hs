{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Alphabets
  --( Password (), VLine (), VTable (), Clear (), Hidden ()
  --, mkPassword, mkCleartext, mkHiddentext
  --, passline, passmatrix
  --, vigengL, gnegivL
  --, canonicallist, samplePass, sampleMatrix,
--
  --)
where

import           Canonical     (Canonical (..), CString)
import           Heretical     (Heretical (..), escape)

import           Data.Bool     (otherwise)
import           Data.Char     (Char)
import           Data.Function (($), (.))
import           Data.List     (drop, elemIndex, filter, head, length, map,
                                (++), concat, cycle, take)
import           Data.Maybe    (fromJust)
import           GHC.Enum      (Bounded (maxBound, minBound),
                                Enum (fromEnum, toEnum))
import           Text.Show     (Show (show))

import           Data.Eq       (Eq ((/=)))
import           GHC.Err       (error)
import           GHC.Num       (Num((-)))

newtype VLine = VLine CString

instance Show VLine where
  show :: VLine -> [Char]
  show (VLine xs) =
    "***ONE VLine***\n"
      ++ escape (show xs) ++ "\n"

newtype VTable = VTable [CString]

instance Show VTable where
  show :: VTable -> [Char]
  show (VTable xs) =
    "***START VTable***\n"
      ++ escape (show (concat xs))
      ++ "\n***END VTable***"

newtype Password = Password CString

instance Show Password where
  show :: Password -> [Char]
  show _ = "***PASSWORD***\n"

mkPassword :: (Heretical a) => [a] -> Password
mkPassword x
  | readableHeresies x =
      Password $ map fromJust $ readHeresies x
  | otherwise = error "mkPassword: Illegal Character in String"

newtype Hidden = Hidden VLine

mkHiddentext :: (Heretical a) => [a] -> Hidden
mkHiddentext x
  | readableHeresies x =
      Hidden $ VLine $ map fromJust $ readHeresies x
  | otherwise = error "mkHiddentext: Illegal Character in String"

instance Show Hidden where
  show :: Hidden -> [Char]
  show (Hidden (VLine [])) = ""
  show (Hidden (VLine (x : xs))) =
    "***START Hidden***\n"
      ++ escape (show [x] ++ show xs)
      ++ "\n***END Hidden***"

newtype Clear = Clear VLine

mkCleartext :: (Heretical a) => [a] -> Clear
mkCleartext x
  | readableHeresies x =
      Clear $ VLine $ map fromJust $ readHeresies x
  | otherwise = error "mkCleartext: Illegal Character in String"

instance Show Clear where
  show :: Clear -> [Char]
  show (Clear (VLine [])) = ""
  show (Clear (VLine (x : xs))) =
    "***START Clear***\n"
      ++ show [x] ++ show xs
      ++ "\n***END Clear***"

canonicallist :: VLine
canonicallist = VLine [minBound .. maxBound]

icanonicallist :: VLine
icanonicallist = VLine $ cycle [minBound .. maxBound]

base85matrix :: VLine -> VTable
base85matrix (VLine a) = passmatrix . VLine $ take 85 a

newtype VCube = VCube [[CString]]

samplePass :: Password
samplePass = Password [LL, LA, LB, LR, LA, LD, LO, LR]

passline :: VLine -> Password -> VLine
passline list (Password []) = list
passline (VLine (x : xs)) (Password (p : assword)) =
  passline (VLine (p : filter (/= p) (x : xs))) (Password assword)
passline _ _ = VLine []

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
    vigengL' :: [CString] -> CString -> CString -> CString -> CString
    vigengL' _ [] _ _ = []
    vigengL' _ _ [] _ = []
    vigengL' matrix (x : xs) (y : ys) p =
      vigeng matrix x y : vigengL' matrix (xs ++ [x]) ys p
    vigeng :: [CString] -> Canonical -> Canonical -> Canonical
    vigeng matrix pass input =
      head $ drop j $ head $ drop i matrix
      where
        i = fromEnum pass
        j = fromEnum input

gnegivL :: VTable -> Hidden -> Password -> Clear
gnegivL (VTable a) (Hidden (VLine b)) (Password c) =
  Clear $ VLine $ gnegivL' a c b c
  where
    gnegivL' :: [CString] -> CString -> CString -> CString -> CString
    gnegivL' _ [] _ _ = []
    gnegivL' _ _ [] _ = []
    gnegivL' matrix (x : xs) (y : ys) p =
      gnegiv matrix x y : gnegivL' matrix (xs ++ [x]) ys p
    gnegiv :: [CString] -> Canonical -> Canonical -> Canonical
    gnegiv matrix pass input =
      toEnum $ fromJust j
      where
        i = fromEnum pass
        j = elemIndex input $ head $ drop i matrix

hide :: Password -> Clear -> Hidden 
hide a b = vigengL m b a
  where
    m = passmatrix $ passline (VLine [minBound..maxBound]) a

reveal :: Password -> Hidden -> Clear
reveal a b = gnegivL m b a
  where
    m = passmatrix $ passline (VLine [minBound..maxBound]) a
