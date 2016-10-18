{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main (main) where

import Data.Time

import Prelude hiding (take)

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import qualified Data.Attoparsec.ByteString.Char8 as P8


import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS


foo_infix =
        char '$' *> signed decimal
        >>=
        (\n -> endOfLine *>take (fromIntegral n) <* endOfLine)

scanLine = signed decimal >>= (\x -> char ',' *> signed decimal >>= (\y -> endOfLine *> pure (x, y) ) )
scanList = many' scanLine


runBench name n f s = do
    t0 <- getCurrentTime
    s `seq` runIt f n
    t1 <- getCurrentTime
    putStrLn $ name ++ " " ++ (show $ diffUTCTime t1 t0)
    where
    runIt f 0 = return ()
    runIt f n = (f s) `seq` runIt f (n-1)
    -- runIt f n = let !x = f s in runIt f (n-1)


main = do
    let n  = 10000000
    putStrLn "Run Benckmark..."
    runBench "Benckmark Redis" n (parse foo_infix) (BS.pack "$4\r\nINFO\r\nTAIL")
    runBench "Benckmark CSV  " n (parse scanList)  (BS.pack "4,5\n2,3\n-")
