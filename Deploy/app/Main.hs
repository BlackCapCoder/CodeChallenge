{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import System.Directory
import Data.List
import Control.Monad
import System.FilePath
import qualified Data.ByteString.Char8 as B
import Data.Char
import Codec.Compression.LZ4

import Encrypt
import History


type Key     = B.ByteString
type SkipKey = Key

encLevels :: [SkipKey] -> IO ()
encLevels ks = do
  fs <- numSort <$> listDirectory "levels"

  forM_ (zip ks fs) $ \(sk, f) -> do
    q <- B.readFile $ "levels" </> f
    B.writeFile ("levels-enc" </> f) $ encrypt sk q

crypt :: History -> [(SkipKey, Key)] -> [B.ByteString -> B.ByteString]
crypt h ks = zipWith (\(s,k) -> \case Beat -> encrypt k; _ -> id) ks h

mkSkipFile :: [(SkipKey, Key)] -> B.ByteString
mkSkipFile ks
  = B.concat $ map (\((s,k), hs) ->
      B.concat $ map (\h ->
        foldl (flip (.)) id (crypt h ks) s
      ) hs
    ) $ zip ks $ histories'' 3

main :: IO ()
main = do
  setCurrentDirectory "/home/blackcap/proj/CodeChallenge/"

  keys     <- B.lines <$> B.readFile "keys"
  skipKeys <- replicateM (length keys) randomKey

  encLevels $ skipKeys

  let sf = mkSkipFile $ zip skipKeys keys
  print $ B.length sf
  -- let Just csf = compress sf
  -- print $ B.length csf
  B.writeFile "skipfile" sf

------

-- Sort a list of strings using leading digits
numSort = sortOn $ (read :: String -> Int) . takeWhile isDigit

