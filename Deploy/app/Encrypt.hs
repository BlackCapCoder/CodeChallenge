{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Encrypt (encrypt, randomKey) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Debug.Trace

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))
import qualified Crypto.Random.Types as CRT
import           Data.ByteArray (ByteArray)

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
  bytes :: B.ByteString <- CRT.getRandomBytes $ blockSize (undefined :: c)
  return $ makeIV bytes

giv :: forall m c. BlockCipher c => c -> Maybe (IV c)
giv _ = makeIV $ B.replicate (blockSize (undefined :: c)) '\0'

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genSecretKey :: forall m c a. (CRT.MonadRandom m, BlockCipher c) => c -> Int -> m B.ByteString
genSecretKey _ = CRT.getRandomBytes

encrypt' :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt' secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    -- Right c -> Right $ ctrCombine c initIV msg
    Right c -> Right $ ctrCombine c initIV msg

-------------

iv :: IV AES256
iv = fromJust $ giv (undefined :: AES256)

encrypt :: B.ByteString -> B.ByteString -> B.ByteString
encrypt k x = case encrypt' (Key $ mappend (B.replicate (32 - B.length k) '\0') k) iv x of
                   Right x -> x
                   Left e -> error $ show e

randomKey :: IO B.ByteString
randomKey = genSecretKey (undefined :: AES256) 32
