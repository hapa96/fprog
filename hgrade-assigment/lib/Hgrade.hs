{-# LANGUAGE OverloadedStrings #-}

-- | This is the Main Module. Starts the Webserver and Generates Demo Files, if User passed -demo as an argument
module Hgrade where

import HGrade.Router as Router
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (createDirectory, doesDirectoryExist, removeDirectoryRecursive)
import System.Environment
import System.Random
import Web.Scotty

-- | Entry Point for Application
main :: IO ()
main = do
  --Read CommandLine Arguments
  args' <- getArgs
  let args = args'
  if null args
    then do
      startWebServer 4000
    else do
      generateDemo $ head args
      startWebServer 4000

-- | Starts the Webserver on the corresponding Port, which is passes as Int
startWebServer :: Int -> IO ()
startWebServer port = do
  scotty port $ do
    middleware logStdoutDev

    get "/" Router.buildIndexHtml

    get "/authors" Router.buildAuthors

    get "/authors/:author" $ do
      paramName <- param "author"
      Router.buildAuthorPage paramName

    get "/grade" Router.buildGrade

    get "/static/styles.css" $ file "static/styles.css"

    post "/grade" $ do
      values <- params
      let author = removeApostrophe $ show $ snd $ head values
      let grader = removeApostrophe $ show $ snd $ head $ tail values
      let gradeValues = removeApostrophe $ concat $ tail $ tail $ map (\(_, y) -> show y) values
      let gradeValuesfinal = "[" ++ parseValues gradeValues ++ "]"
      Router.addNewGrader author grader gradeValuesfinal

-- | Takes a String and returns the String without Apostrophes
removeApostrophe :: String -> String
removeApostrophe [] = []
removeApostrophe (x : xs)
  | x == '"' = removeApostrophe xs
  | otherwise = x : removeApostrophe xs

-- | Takes a String and returns the String in the desired representation => "[1,1,1,1,2]"
parseValues :: String -> String
parseValues [] = []
parseValues [x] = x : parseValues []
parseValues (x : xs) = [x] ++ "," ++ parseValues xs

-- | Checks, if a user entered the argument "-demo". If true, the Program genereates randomly demo files. If the user entered an invalid argument, print a instructions
generateDemo :: String -> IO ()
generateDemo "-demo" = do
  numberOfAuthors <- generateNumber 3 10
  doesAlreadyExist <- doesDirectoryExist "data/"
  if doesAlreadyExist
    then --If data/ already exists, remove whole folder and create a new one with demo files
    do
      removeDirectoryRecursive "data/"
      createDirectory "data/"
    else --data/ does not exist yet. -> No need to remove any folder
    do
      createDirectory "data/"
  generateDemoHelper numberOfAuthors --generate random amount of Authors
generateDemo "" = return ()
--Print a Instruction, If User enters an invalid Argument
generateDemo x = putStrLn $ "\n" ++ x ++ " is not a valid Command Line Argument \n" ++ "Valid Arguments:I\n" ++ "-demo: Randomly generates demo files  I\n"

-- | Generates Demo Files. Takes an Int Value, which represents the number of Folders, that should be generated.
generateDemoHelper :: Int -> IO ()
generateDemoHelper 0 = return ()
generateDemoHelper n = do
  numberOfGrader <- generateNumber 1 10
  let path = "data/author" ++ show n
  createDirectory path
  generateGrader path numberOfGrader
  generateDemoHelper $ n -1

-- | Generates a Grader. Takes a Filepath and an Integers, which stands for the number of Graders.
generateGrader :: FilePath -> Int -> IO ()
generateGrader _ 0 = return ()
generateGrader path n = do
  grades <- generateGrades (length Router.topics)
  writeFile (path ++ "/" ++ "grader" ++ show n ++ ".txt") $ show grades
  generateGrader path (n -1)

-- | Takes an Integers, which stands for number of Topics and  generates random List of Integers. [0,2]
generateGrades :: Int -> IO [Int]
generateGrades n = do
  mapM (\_ -> generateNumber 0 2) $ (replicate n 0)

-- | Generates a Random Number. Takes a min and max Value and returns a generated Number within this Boundaries
generateNumber :: Int -> Int -> IO Int
generateNumber minValue maxValue = do
  rand <- randomRIO (minValue, maxValue) :: IO Int
  let number = rand
  return $! number