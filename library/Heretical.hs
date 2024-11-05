{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase   #-}

module Heretical 
  --(Heretical (..)) 
where

import           Canonical     (Canonical (..), CString)

import           Data.Bool     (Bool, otherwise)
import           Data.Char     (Char)
import           Data.Function (($), (.))
import           Data.List     (head, null, notElem, map)
import           Data.Maybe    (Maybe (Just, Nothing), fromJust, isNothing)
import           Data.Tuple    (fst)
import Text.Read (Read(..))


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
  readHeresies []       = []
  
  readableHeresy :: a -> Bool
  readableHeresy x = readableHeresies [x]
  
  readableHeresies :: [a] -> Bool
  readableHeresies x = notElem Nothing (readHeresies x)

rescape :: [Char] -> [Char]
rescape = map (\case '#' -> '\n'; '_' -> ' '; x -> x)

escape :: [Char] -> [Char]
escape = map (\case '\n' -> '#'; ' ' -> '_'; x -> x)
 
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
  heresyParser x              = ([]       , x )