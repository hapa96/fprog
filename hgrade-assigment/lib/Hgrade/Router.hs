-- | The Router builds all the required HTML Pages and route it
module HGrade.Router where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import HGrade.Calculation as CALC
import HGrade.GenerateHTML as HTML
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import Web.Scotty

-- | Define a Type for Grader = (Name,[Grades])
type Grader = (FilePath, [Int])

-- | Pre defined Topics to fulfill criteria (N9)
topics :: [String]
topics = ["N1", "N3", "F1", "F2", "F3", "F4"]

-- | Build Author Page
buildAuthorPage :: String -> ActionM ()
buildAuthorPage param' = do
  let author = param'
  grader <- liftIO $ getGrader $ "data/" ++ author
  let graderList = if null grader then [] else grader
  html
    ( T.pack $
        HTML.render
          [ HTML.H1 $ "Author: " ++ author,
            HTML.Table $ [HTML.Th (" " : topics)] ++ generateAuthorPage graderList ++ [HTML.Td ("Median" : medianBuilder graderList)] ++ [HTML.Histogram $ histogramBuilder graderList]
          ]
    )

-- | Generates the Table with all the Graders for an Author
generateAuthorPage :: [Grader] -> [Html]
generateAuthorPage [] = []
generateAuthorPage (x : xs) = generateAuthorPageHelper x : generateAuthorPage xs
  where
    generateAuthorPageHelper (name, grades) = HTML.Tr [HTML.Td $ name : map show grades]

-- | Gets all the Graders of a specific Author
getGrader :: FilePath -> IO [Grader]
getGrader filePath = do
  list <- listDirectory filePath
  let fileList = list
  let filesWithPath = buildFullPath (filePath ++ "/") fileList
  values' <- mapM readFile filesWithPath
  let values = map stringToIntArray values'
  let fileListFinal = map finalizeGraderName fileList
  let res = zip fileListFinal values
  return $! res

-- | Create new Grader and eventually a new Author (if not already exist) to the Filesystem. Invoked, when a new POST requests arrives.
addNewGrader :: String -> String -> String -> ActionM ()
addNewGrader author grader values = do
  dataDoesExist <- liftIO $ doesDirectoryExist "data/"
  authorDoesExist <- liftIO $ doesDirectoryExist ("data/" ++ author ++ "/")
  if not dataDoesExist
    then do
      liftIO $ createDirectory "data/"
      liftIO $ createDirectory ("data/" ++ author ++ "/")
      liftIO $ writeFile ("data/" ++ author ++ "/" ++ grader ++ ".txt") values
    else
      if authorDoesExist
        then --Create a new File within the existing folder for the new grader
          liftIO $ writeFile ("data/" ++ author ++ "/" ++ grader ++ ".txt") values
        else do
          --Create a new Folder for the new created Author and a new File for the new grader
          liftIO $ createDirectory ("data/" ++ author ++ "/")
          liftIO $ writeFile ("data/" ++ author ++ "/" ++ grader ++ ".txt") values
  buildAuthors

-- | Converts a String with the following representation "[1,2,3,4]" to an List of Doubles
stringToIntArray :: String -> [Int]
stringToIntArray [] = []
stringToIntArray (x : xs)
  | x == ',' = stringToIntArray xs
  | x == '[' = stringToIntArray xs
  | x == ']' = stringToIntArray xs
  | otherwise = (read [x] :: Int) : stringToIntArray xs

-- | Removes the file extension from a File -> removes everything after the character '.'
finalizeGraderName :: String -> String
finalizeGraderName [] = []
finalizeGraderName (x : xs)
  | x == '.' = []
  | otherwise = x : finalizeGraderName xs

-- | Takes a File and appends the Full Path to it
buildFullPath :: FilePath -> [FilePath] -> [FilePath]
buildFullPath _ [] = []
buildFullPath path (x : xs) = (path ++ x) : buildFullPath path xs

-- | Build the Index Page
buildIndexHtml :: ActionM ()
buildIndexHtml = html (T.pack $ HTML.render [HTML.H1 "Hgrade- Peergrading in Haskell", HTML.Ul [HTML.Href "/authors" "Grading Overview", HTML.Href "/grade" "Submit Grading"]])

-- | Build the Authors Overview
buildAuthors :: ActionM ()
buildAuthors = do
  dataDoesExist <- liftIO $ doesDirectoryExist "data/"
  if not dataDoesExist
    then do
      html (T.pack $ HTML.render [HTML.H1 "Data Folder does not exist!"])
    else do
      list <- liftIO $ getAuthorList "data/"
      let authorlist = if null list then [] else list
      html (T.pack $ HTML.render [HTML.H1 "Grade", HTML.Ul $ map (\x -> HTML.Href ("/authors/" ++ x) x) authorlist])

-- | Build the Grade Page
buildGrade :: ActionM ()
buildGrade = html (T.pack $ HTML.render [HTML.H1 "Grade", HTML.Form $ mergeToLists ["Author", "Grader"] topics])

-- | Get all Authors in the Folder
getAuthorList :: FilePath -> IO [FilePath]
getAuthorList filePath = do
  list <- listDirectory filePath
  let res = list
  return $! res

-- | Takes a Tuple and returns it as a list
pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

-- | Merge two Lists and merge it to one. First the whole List 01 and then the List 02
mergeToLists :: [a] -> [a] -> [a]
mergeToLists xs [] = xs
mergeToLists [] ys = ys
mergeToLists (x : xs) ys = x : mergeToLists xs ys

-- | Transforms a Tuple into an 2D-List to calculate the Median
tupleToListOfGrades :: [Grader] -> [[Int]]
tupleToListOfGrades graderList = CALC.colsToRows $ map snd graderList

-- | Builds the Median with the given Graders
medianBuilder :: [Grader] -> [String]
medianBuilder = map show . CALC.buildAllMedians . tupleToListOfGrades

-- | Builds the Histogram with the given Graders
histogramBuilder :: [Grader] -> [(Int, Int, Int)]
histogramBuilder = CALC.buildAllHistograms . tupleToListOfGrades
