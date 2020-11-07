module Main where

import System.Environment
import Network.HTTP

-- cabal repl
-- :main Arg1 Arg2
-- :set args Arg1 Arg2
-- cabal run webclient FirstName LastName
main :: IO ()
main = do
    putStrLn "Load a joke from the web!"
