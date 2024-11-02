{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Canonical (Canonical (..)) where

import           Data.Char     (Char)
import           Data.Eq       (Eq)
import           Data.List     ((++))
import           Data.Maybe    (Maybe (Just, Nothing))
import           GHC.Enum      (Bounded, Enum)
import           Text.Show     (Show (show))

data Canonical
  = LG | LH | LI | LJ | LK | LL | LA    | N0 | N1 | N2 | N3 | N4
  | LM | LB | LN | LO | LP | LQ | LR    | N5 | N6 | N7 | N8 | N9
  | LS | LT | LC | LU | LV | LW | LX 
  | LY | LZ | LD | LE | LF 
                                        
  | Lb | Lk | Ll | Lm | Ln              | WEOL | WSPACE
  | Lc | Lh | Li | Lj                   | SQMARK | SEXMARK | SSTOP 
  | La | Lo | Lp | Lq | Lr              | SCOMMA | SDASH 
  | Ld | Le | Lf | Lg
  | Ls | Lt | Lu | Lz
  | Lv | Lw | Lx | Ly
  deriving (Eq, Enum, Bounded, Show)

instance {-# OVERLAPPING #-} Show [Canonical] where
  show :: [ Canonical] -> [Char]
  --empty input gives empty output
  show []             =   ""
  show (LA : xs)      =  "A" ++ show xs
  show (LB : xs)      =  "B" ++ show xs
  show (LC : xs)      =  "C" ++ show xs
  show (LD : xs)      =  "D" ++ show xs
  show (LE : xs)      =  "E" ++ show xs
  show (LF : xs)      =  "F" ++ show xs
  show (LG : xs)      =  "G" ++ show xs
  show (LH : xs)      =  "H" ++ show xs
  show (LI : xs)      =  "I" ++ show xs
  show (LJ : xs)      =  "J" ++ show xs
  show (LK : xs)      =  "K" ++ show xs
  show (LL : xs)      =  "L" ++ show xs
  show (LM : xs)      =  "M" ++ show xs
  show (LN : xs)      =  "N" ++ show xs
  show (LO : xs)      =  "O" ++ show xs
  show (LP : xs)      =  "P" ++ show xs
  show (LQ : xs)      =  "Q" ++ show xs
  show (LR : xs)      =  "R" ++ show xs
  show (LS : xs)      =  "S" ++ show xs
  show (LT : xs)      =  "T" ++ show xs
  show (LU : xs)      =  "U" ++ show xs
  show (LV : xs)      =  "V" ++ show xs
  show (LW : xs)      =  "W" ++ show xs
  show (LX : xs)      =  "X" ++ show xs
  show (LY : xs)      =  "Y" ++ show xs
  show (LZ : xs)      =  "Z" ++ show xs
  show (La : xs)      =  "a" ++ show xs
  show (Lb : xs)      =  "b" ++ show xs
  show (Lc : xs)      =  "c" ++ show xs
  show (Ld : xs)      =  "d" ++ show xs
  show (Le : xs)      =  "e" ++ show xs
  show (Lf : xs)      =  "f" ++ show xs
  show (Lg : xs)      =  "g" ++ show xs
  show (Lh : xs)      =  "h" ++ show xs
  show (Li : xs)      =  "i" ++ show xs
  show (Lj : xs)      =  "j" ++ show xs
  show (Lk : xs)      =  "k" ++ show xs
  show (Ll : xs)      =  "l" ++ show xs
  show (Lm : xs)      =  "m" ++ show xs
  show (Ln : xs)      =  "n" ++ show xs
  show (Lo : xs)      =  "o" ++ show xs
  show (Lp : xs)      =  "p" ++ show xs
  show (Lq : xs)      =  "q" ++ show xs
  show (Lr : xs)      =  "r" ++ show xs
  show (Ls : xs)      =  "s" ++ show xs
  show (Lt : xs)      =  "t" ++ show xs
  show (Lu : xs)      =  "u" ++ show xs
  show (Lv : xs)      =  "v" ++ show xs
  show (Lw : xs)      =  "w" ++ show xs
  show (Lx : xs)      =  "x" ++ show xs
  show (Ly : xs)      =  "y" ++ show xs
  show (Lz : xs)      =  "z" ++ show xs
  show (N0 : xs)      =  "0" ++ show xs
  show (N1 : xs)      =  "1" ++ show xs
  show (N2 : xs)      =  "2" ++ show xs
  show (N3 : xs)      =  "3" ++ show xs
  show (N4 : xs)      =  "4" ++ show xs
  show (N5 : xs)      =  "5" ++ show xs
  show (N6 : xs)      =  "6" ++ show xs
  show (N7 : xs)      =  "7" ++ show xs
  show (N8 : xs)      =  "8" ++ show xs
  show (N9 : xs)      =  "9" ++ show xs
  show (SQMARK : xs)  =  "?" ++ show xs
  show (SEXMARK : xs) =  "!" ++ show xs
  show (SSTOP : xs)   =  "." ++ show xs
  show (SCOMMA : xs)  =  "," ++ show xs
  show (SDASH : xs)   =  "-" ++ show xs
  show (WSPACE : xs)  =  " " ++ show xs
  show (WEOL : xs)    =  "\n" ++ show xs

instance {-# OVERLAPPING #-} Show [Maybe Canonical] where
  show :: [Maybe Canonical] -> [Char]
  --Parse Error is displayed as #
  show (Nothing        : xs) =  "~" ++ show xs
  --empty input gives this output
  show []                    = ""
  --alternative display options for space and nl
  show ((Just WSPACE ) : xs) = "_" ++ show xs
  show ((Just WEOL   ) : xs) = "#" ++ show xs
  show ((Just x     )  : xs) = show [x] ++ show xs