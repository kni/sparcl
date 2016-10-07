{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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


runIt f s 0 = BS.putStrLn "Run Done"
runIt f s n = (f s) `seq` runIt f s (n-1)
-- runIt f s n = let !x = f s in runIt f s (n-1)


main = do
    testResult (runParser (takeStr "INFO") "INFOTAIL") (Done "INFO" "TAIL") "takeStr"
    testResult (runParser (takeBefore "TAIL") "INFOTAIL") (Done "INFO" "TAIL") "takeBefore"

    testResult (runParser foo "$4\r\nINFO\r\nTAIL") (Done "INFO" "TAIL") "foo"
    testResult (runParser foo_infix "$4\r\nINFO\r\nTAIL") (Done "INFO" "TAIL") "foo_infix"
    testResult (runParser (choice [(takeStr "PING"), (takeStr "INFO")]) "INFOTAIL") (Done "INFO" "TAIL") "choice"

    putStrLn "Run Benckmark..."
    runIt (\s -> runParser foo_infix s) (BS.pack "$4\r\nINFO\r\nTAIL") 10000000
