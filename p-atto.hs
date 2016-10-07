{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main (main) where

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


runIt f s 0 = BS.putStrLn "Run Done"
runIt f s n = (f s) `seq` runIt f s (n-1)
-- runIt f s n = let !x = f s in runIt f s (n-1)


main = do
	runIt (\s -> parse foo_infix s) (BS.pack "$4\r\nINFO\r\nTAIL") 10000000
	BS.putStrLn "The End"
