{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Time

import Sparcl

import Control.Applicative (Applicative(..), (<$>))

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS


resultShow :: Result ByteString -> IO ()
resultShow (Done r t) = BS.putStr r >> BS.putStr ", " >> BS.putStrLn t
resultShow Partial    = BS.putStrLn "Partial"
resultShow Fail       = BS.putStrLn "Fail"


compareResult (Done r1 t1) (Done r2 t2) = if r1 == r2 && t1 == t2 then True else False
compareResult Partial         Partial   = True
compareResult Fail            Fail      = True
compareResult _               _         = False

testResult r expected name =
  if compareResult r expected
  then putStrLn $ "OK - " ++ name
  else putStrLn $ "Not OK - " ++ name

foo = (
    bind
        (apR (takeStr "$") takeInt)
        (\ n ->
            (apR (takeStr "\r\n") (
                (apL (takeN n) (takeStr "\r\n"))
            ))
        )
    )


foo_infix =
        takeStr "$" *> takeInt
        >>=
        (\n -> takeStr "\r\n" *> takeN n <* takeStr "\r\n")



scanLine = takeInt >>= (\x -> takeStr "," *> takeInt >>= (\y -> takeStr "\n" *> pure (x, y) ) )
scanList = many scanLine



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
    testResult (runParser (takeStr "INFO") "INFOTAIL") (Done "INFO" "TAIL") "takeStr"
    testResult (runParser (takeBefore "TAIL") "INFOTAIL") (Done "INFO" "TAIL") "takeBefore"

    testResult (runParser foo "$4\r\nINFO\r\nTAIL") (Done "INFO" "TAIL") "foo"
    testResult (runParser foo_infix "$4\r\nINFO\r\nTAIL") (Done "INFO" "TAIL") "foo_infix"
    testResult (runParser (choice [(takeStr "PING"), (takeStr "INFO")]) "INFOTAIL") (Done "INFO" "TAIL") "choice"

    let n = 10000000
    putStrLn "Run Benckmark..."
    runBench "Benckmark Redis" n (runParser foo_infix) (BS.pack "$4\r\nINFO\r\nTAIL")
    runBench "Benckmark CSV  " n (runParser scanList)  (BS.pack "4,5\n2,3\n-")
