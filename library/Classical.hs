{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Classical
  --(Classical (..))
where

import           Data.Word
import           Canonical       (CString, Canonical)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

class Classical a where
    serialize :: a -> ByteString
    deserialize :: ByteString -> a

instance Classical CString where
    serialize :: CString -> ByteString
    serialize a = BS.pack $ Prelude.map fromIntegral $ serialize' a
      where
        serialize' :: CString -> [Int]
        serialize' [] = []
        serialize' (x: xs) = 
            fromIntegral (fromEnum x)  : serialize' xs

    deserialize :: ByteString -> CString
    deserialize a = 
        deserialize' $ map fromIntegral $ BS.unpack a
      where
        deserialize' :: [Int] -> CString
        deserialize' [] = []
        deserialize' (x: xs) = 
            toEnum (fromIntegral x)  : deserialize' xs

instance Classical Canonical where
    serialize :: Canonical -> ByteString
    serialize x = 
        BS.singleton $ fromIntegral $ fromEnum x

    deserialize :: ByteString -> Canonical
    deserialize x
      |  BS.length x == 1 = 
            toEnum $ fromIntegral $ head $ BS.unpack x
      |  otherwise = 
            error "Bytestring of length > 1 is not allowed"

wordlist :: [Word8]
wordlist = [minBound..maxBound]