module Main where

import Network.HTTP
import System.Environment

-- cabal repl
-- :main Arg1 Arg2
-- :set args Arg1 Arg2
-- cabal run webclient FirstName LastName
main :: IO ()
main = do
  [first, second] <- getArgs
  let url = "http://api.icndb.com/jokes/random?limitTo=%5Bnerdy%5D&firstName=" ++ first ++ "&lastName=" ++ second
  let req = getRequest url
  res <- simpleHTTP req
  content <- getResponseBody res
  putStrLn content

