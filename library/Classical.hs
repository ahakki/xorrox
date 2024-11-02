{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Classical 
  --(Classical (..))
where

import Canonical ( Canonical , CString )

class Classical a where
    serialize :: a -> [Int]

    deserialize :: [Int] -> a

instance Classical CString where
    serialize :: CString -> [Int]
    serialize []      = []
    serialize (x: xs) = 33 + fromEnum x  : serialize xs

    deserialize :: [Int] -> CString
    deserialize [] = []
    deserialize (x: xs) = toEnum (x - 33)  : deserialize xs

instance Classical Canonical where
    serialize :: Canonical -> [Int]
    serialize x = [33 + fromEnum x]
    
    deserialize :: [Int] -> Canonical
    deserialize [] = error "instance Classical Canonical needs a single input"
    deserialize (x:_) = toEnum (x - 33)