{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Canonical where

import Data.Maybe (isNothing, fromJust, Maybe (Nothing, Just))
import Data.Eq (Eq)
import GHC.Enum (Enum, Bounded)
import Data.Char (Char)
import Data.Bool (otherwise)
import Data.List (null, head, (++))
import Data.Tuple (fst)
import Data.Function (($), (.))
import Text.Show (Show (show))

data Canonical
  --Numerals
  = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
  --Whitespace
  | WEOL | WSPACE
  --Special
  | SQMARK | SEXMARK | SSTOP | SCOMMA | SDASH
  --Uppercase
  | LA | LB | LC | LD | LE | LF | LG | LH | LI | LJ | LK | LL | LM
  | LN | LO | LP | LQ | LR | LS | LT | LU | LV | LW | LX | LY | LZ
  --Lowercase
  | La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm
  | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | Lv | Lw | Lx | Ly | Lz
  deriving (Eq, Enum, Bounded)

instance {-# OVERLAPPING #-} Show [Canonical] where
  show :: [ Canonical] -> [Char]
  --empty input gives empty output
  show []                    =   ""
  show (LA : xs) =  "A" ++ show xs
  show (LB : xs) =  "B" ++ show xs
  show (LC : xs) =  "C" ++ show xs
  show (LD : xs) =  "D" ++ show xs
  show (LE : xs) =  "E" ++ show xs
  show (LF : xs) =  "F" ++ show xs
  show (LG : xs) =  "G" ++ show xs
  show (LH : xs) =  "H" ++ show xs
  show (LI : xs) =  "I" ++ show xs
  show (LJ : xs) =  "J" ++ show xs
  show (LK : xs) =  "K" ++ show xs
  show (LL : xs) =  "L" ++ show xs
  show (LM : xs) =  "M" ++ show xs
  show (LN : xs) =  "N" ++ show xs
  show (LO : xs) =  "O" ++ show xs
  show (LP : xs) =  "P" ++ show xs
  show (LQ : xs) =  "Q" ++ show xs
  show (LR : xs) =  "R" ++ show xs
  show (LS : xs) =  "S" ++ show xs
  show (LT : xs) =  "T" ++ show xs
  show (LU : xs) =  "U" ++ show xs
  show (LV : xs) =  "V" ++ show xs
  show (LW : xs) =  "W" ++ show xs
  show (LX : xs) =  "X" ++ show xs
  show (LY : xs) =  "Y" ++ show xs
  show (LZ : xs) =  "Z" ++ show xs
  show (La : xs) =  "a" ++ show xs
  show (Lb : xs) =  "b" ++ show xs
  show (Lc : xs) =  "c" ++ show xs
  show (Ld : xs) =  "d" ++ show xs
  show (Le : xs) =  "e" ++ show xs
  show (Lf : xs) =  "f" ++ show xs
  show (Lg : xs) =  "g" ++ show xs
  show (Lh : xs) =  "h" ++ show xs
  show (Li : xs) =  "i" ++ show xs
  show (Lj : xs) =  "j" ++ show xs
  show (Lk : xs) =  "k" ++ show xs
  show (Ll : xs) =  "l" ++ show xs
  show (Lm : xs) =  "m" ++ show xs
  show (Ln : xs) =  "n" ++ show xs
  show (Lo : xs) =  "o" ++ show xs
  show (Lp : xs) =  "p" ++ show xs
  show (Lq : xs) =  "q" ++ show xs
  show (Lr : xs) =  "r" ++ show xs
  show (Ls : xs) =  "s" ++ show xs
  show (Lt : xs) =  "t" ++ show xs
  show (Lu : xs) =  "u" ++ show xs
  show (Lv : xs) =  "v" ++ show xs
  show (Lw : xs) =  "w" ++ show xs
  show (Lx : xs) =  "x" ++ show xs
  show (Ly : xs) =  "y" ++ show xs
  show (Lz : xs) =  "z" ++ show xs
  show (N0 : xs) =  "0" ++ show xs
  show (N1 : xs) =  "1" ++ show xs
  show (N2 : xs) =  "2" ++ show xs
  show (N3 : xs) =  "3" ++ show xs
  show (N4 : xs) =  "4" ++ show xs
  show (N5 : xs) =  "5" ++ show xs
  show (N6 : xs) =  "6" ++ show xs
  show (N7 : xs) =  "7" ++ show xs
  show (N8 : xs) =  "8" ++ show xs
  show (N9 : xs) =  "9" ++ show xs
  show (SQMARK : xs) =  "?" ++ show xs
  show (SEXMARK : xs) =  "!" ++ show xs
  show (SSTOP : xs) =  "." ++ show xs
  show (SCOMMA : xs) =  "," ++ show xs
  show (SDASH : xs) = "-" ++ show xs
  show (WSPACE : xs) =  "_" ++ show xs
  show (WEOL : xs) =  "~\n" ++ show xs

instance {-# OVERLAPPING #-} Show [Maybe Canonical] where
  show :: [Maybe Canonical] -> [Char]
  -- Parse Error is displayed as #
  show (Nothing        : xs) =  "#" ++ show xs
  --empty input gives this output
  show []                    = ""
  show ((Just WSPACE ) : xs) = "_" ++ show xs
  show ((Just WEOL   ) : xs) = "~" ++ show xs
  show ((Just x     ) : xs)  = show [x] ++ show xs

class Heretical a where
  heresyParser :: [a] -> ([Canonical], [a])
  heresyParser (x : xs)
    | isNothing (readHeresy x) = ([], x : xs)
    | otherwise = ([(fromJust . readHeresy) x], xs)
  heresyParser [] = ([], [])

  readHeresy :: a -> Maybe Canonical
  readHeresy x
    | null (fst (heresyParser [x])) = Nothing
    | otherwise = conv x
    where
      conv y = Just $ head . fst . heresyParser $ [y]

  readHeresies :: [a] -> [Maybe Canonical]
  readHeresies (x : xs) = readHeresy x : readHeresies xs
  readHeresies [] = []

instance Heretical Char where
  heresyParser :: [Char] -> ([Canonical], [Char])
  heresyParser []             = ([]       , []  )
  heresyParser ( 'A'  : rest) = ([LA]     , rest)
  heresyParser ( 'B'  : rest) = ([LB]     , rest)
  heresyParser ( 'C'  : rest) = ([LC]     , rest)
  heresyParser ( 'D'  : rest) = ([LD]     , rest)
  heresyParser ( 'E'  : rest) = ([LE]     , rest)
  heresyParser ( 'F'  : rest) = ([LF]     , rest)
  heresyParser ( 'G'  : rest) = ([LG]     , rest)
  heresyParser ( 'H'  : rest) = ([LH]     , rest)
  heresyParser ( 'I'  : rest) = ([LI]     , rest)
  heresyParser ( 'J'  : rest) = ([LJ]     , rest)
  heresyParser ( 'K'  : rest) = ([LK]     , rest)
  heresyParser ( 'L'  : rest) = ([LL]     , rest)
  heresyParser ( 'M'  : rest) = ([LM]     , rest)
  heresyParser ( 'N'  : rest) = ([LN]     , rest)
  heresyParser ( 'O'  : rest) = ([LO]     , rest)
  heresyParser ( 'P'  : rest) = ([LP]     , rest)
  heresyParser ( 'Q'  : rest) = ([LQ]     , rest)
  heresyParser ( 'R'  : rest) = ([LR]     , rest)
  heresyParser ( 'S'  : rest) = ([LS]     , rest)
  heresyParser ( 'T'  : rest) = ([LT]     , rest)
  heresyParser ( 'U'  : rest) = ([LU]     , rest)
  heresyParser ( 'V'  : rest) = ([LV]     , rest)
  heresyParser ( 'W'  : rest) = ([LW]     , rest)
  heresyParser ( 'X'  : rest) = ([LX]     , rest)
  heresyParser ( 'Y'  : rest) = ([LY]     , rest)
  heresyParser ( 'Z'  : rest) = ([LZ]     , rest)
  heresyParser ( 'a'  : rest) = ([La]     , rest)
  heresyParser ( 'b'  : rest) = ([Lb]     , rest)
  heresyParser ( 'c'  : rest) = ([Lc]     , rest)
  heresyParser ( 'd'  : rest) = ([Ld]     , rest)
  heresyParser ( 'e'  : rest) = ([Le]     , rest)
  heresyParser ( 'f'  : rest) = ([Lf]     , rest)
  heresyParser ( 'g'  : rest) = ([Lg]     , rest)
  heresyParser ( 'h'  : rest) = ([Lh]     , rest)
  heresyParser ( 'i'  : rest) = ([Li]     , rest)
  heresyParser ( 'j'  : rest) = ([Lj]     , rest)
  heresyParser ( 'k'  : rest) = ([Lk]     , rest)
  heresyParser ( 'l'  : rest) = ([Ll]     , rest)
  heresyParser ( 'm'  : rest) = ([Lm]     , rest)
  heresyParser ( 'n'  : rest) = ([Ln]     , rest)
  heresyParser ( 'o'  : rest) = ([Lo]     , rest)
  heresyParser ( 'p'  : rest) = ([Lp]     , rest)
  heresyParser ( 'q'  : rest) = ([Lq]     , rest)
  heresyParser ( 'r'  : rest) = ([Lr]     , rest)
  heresyParser ( 's'  : rest) = ([Ls]     , rest)
  heresyParser ( 't'  : rest) = ([Lt]     , rest)
  heresyParser ( 'u'  : rest) = ([Lu]     , rest)
  heresyParser ( 'v'  : rest) = ([Lv]     , rest)
  heresyParser ( 'w'  : rest) = ([Lw]     , rest)
  heresyParser ( 'x'  : rest) = ([Lx]     , rest)
  heresyParser ( 'y'  : rest) = ([Ly]     , rest)
  heresyParser ( 'z'  : rest) = ([Lz]     , rest)
  heresyParser ( '0'  : rest) = ([N0]     , rest)
  heresyParser ( '1'  : rest) = ([N1]     , rest)
  heresyParser ( '2'  : rest) = ([N2]     , rest)
  heresyParser ( '3'  : rest) = ([N3]     , rest)
  heresyParser ( '4'  : rest) = ([N4]     , rest)
  heresyParser ( '5'  : rest) = ([N5]     , rest)
  heresyParser ( '6'  : rest) = ([N6]     , rest)
  heresyParser ( '7'  : rest) = ([N7]     , rest)
  heresyParser ( '8'  : rest) = ([N8]     , rest)
  heresyParser ( '9'  : rest) = ([N9]     , rest)
  heresyParser ( '?'  : rest) = ([SQMARK ], rest)
  heresyParser ( '!'  : rest) = ([SEXMARK], rest)
  heresyParser ( '.'  : rest) = ([SSTOP  ], rest)
  heresyParser ( ','  : rest) = ([SCOMMA ], rest)
  heresyParser ( '-'  : rest) = ([SDASH], rest)
  heresyParser ( ' '  : rest) = ([WSPACE ], rest)
  heresyParser ( '\n' : rest) = ([WEOL   ], rest)
  heresyParser x            = ([]       , x )