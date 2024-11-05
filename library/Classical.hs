{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Classical
  --(Classical (..))
where

import           Canonical       (CString, Canonical)
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List       (elemIndex)
import           Data.Maybe      (fromJust)
import           Data.Word
import           SoupKitchen


class Classical a where
    serialize :: a -> [Word8]
    deserialize :: [Word8] -> a

instance Classical CString where

    serialize :: CString -> [Word8]
    serialize [] = []
    serialize (x: xs) =
        fromIntegral (fromEnum x)  : serialize xs

    deserialize :: [Word8] -> CString
    deserialize [] = []
    deserialize (x: xs) =
        toEnum (fromIntegral x)  : deserialize xs

instance Classical Canonical where
    serialize :: Canonical -> [Word8]
    serialize x = [fromIntegral $ fromEnum x]

    deserialize :: [Word8] -> Canonical
    deserialize x
      |  length x == 1 =
            toEnum $ fromIntegral $ head x
      |  otherwise =
            error "List Word8 of length > 1 is not allowed"

wordlist :: [Word8]
wordlist = [minBound..maxBound]

wordpasslist :: [Word8] -> [Word8] -> [Word8]
wordpasslist list [] = list
wordpasslist (x : xs) (p : ps) =
  wordpasslist ( p : filter (/= p) (x : xs)) ps
wordpasslist _ _ =  []

wordmatrix :: [Word8] -> [[Word8]]
wordmatrix a = passmatrix' [] a (length a)
  where
    passmatrix' acc _ 0 = acc
    passmatrix' acc (x : xs) i =
      passmatrix' (acc ++ [x : xs]) (xs ++ [x]) (i - 1)
    passmatrix' _ _ _ = []

wordcube :: [[Word8]] -> [[[Word8]]]
wordcube = map wordmatrix


iwordcube ::  [[[Word8]]] ->  [[[Word8]]]
iwordcube x = cycle x

wordlistreveal :: [[[Word8]]] -> [Word8] -> [Word8] -> [Word8]
wordlistreveal a b c =
  wordreveal' a c b c
  where
    wordreveal' _ [] _ _ = []
    wordreveal' _ _ [] _ = []
    wordreveal' (m:ms) (x : xs) (y : ys) p =
      onewordreveal m x y : wordreveal' ms (xs ++ [x]) ys p
    wordreveal' _ _ _ _ = []

onewordreveal :: [[Word8]] -> Word8 -> Word8 -> Word8
onewordreveal matrix pass input =
  fromIntegral $ fromJust j
  where
    i = fromIntegral pass
    j = elemIndex input $ head $ drop i matrix

wordlisthide ::  [[[Word8]]]-> [Word8]  -> [Word8] -> [Word8]
wordlisthide a b c =
    wordhide' a c b c
  where
    wordhide' _ [] _ _ = []
    wordhide' _ _ [] _ = []
    wordhide' (m:ms) (x : xs) (y : ys) p =
      onewordhide m x y : wordhide' ms (xs ++ [x]) ys p
    wordhide' _ _ _ _ = []

onewordhide :: [[Word8]] -> Word8 -> Word8 -> Word8
onewordhide matrix pass input =
  head $ drop j $ head $ drop i matrix
  where
    i = fromEnum pass
    j = fromEnum input

hideC :: CString -> CString -> ByteString
hideC x y = output
  where
    password = serialize x
    input = zipWith xor rlistC $ serialize y
    cube = iwordcube $ wordcube $ wordmatrix $ wordpasslist wordlist password
    seed = mod (maxBound::Int) $ sum (map fromIntegral password)
    rlistC = randSeed8 seed
    rlist8 = randSeedC seed
    output = BS.pack $
        zipWith xor rlist8 $ wordlisthide cube input password

revealC :: CString -> ByteString -> CString
revealC x y = output
  where
    password = serialize x
    input = zipWith xor rlist8 $ BS.unpack y
    cube = iwordcube $ wordcube $ wordmatrix $ wordpasslist wordlist password
    seed = mod (maxBound::Int) $ product (map fromIntegral password)
    rlistC = randSeed8 seed
    rlist8 = randSeedC seed
    output:: CString
    output = deserialize $
        zipWith xor rlistC $ wordlistreveal cube input password

hide8 :: [Word8] -> ByteString -> ByteString
hide8 x y = output
  where 
    password = x
    input = zipWith xor rlist8 y
    cube = iwordcube $ wordcube $ wordmatrix $ wordpasslist wordlist password
    seed = mod (maxBound::Int) $ sum (map fromIntegral password)
    rlist8 = randSeedC seed
    output = BS.pack $
        zipWith xor rlist8 $ wordlisthide cube input password

reveal8 :: [Word8] -> ByteString -> ByteString
reveal8 x y = output
  where
    password = x
    input = zipWith xor rlist8 $ BS.unpack y
    cube = iwordcube $ wordcube $ wordmatrix $ wordpasslist wordlist password
    seed = mod (maxBound::Int) $ product (map fromIntegral password)
    rlist8 = randSeedC seed
    output = BS.pack $ 
        zipWith xor rlist8 $ wordlistreveal cube input password
