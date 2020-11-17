-- Implementation from Assignment AS_TaskList.pdf
import System.Directory (doesFileExist)

type Task = (Bool, String)

type TaskList = [Task]

-- a
readTasks :: FilePath -> IO TaskList
readTasks file = do
  exists <- doesFileExist file
  if not exists
    then return []
    else do
      content <- readFile file
      let tasks = if null content then [] else read content
      return $! tasks

writeTasks :: FilePath -> TaskList -> IO ()
writeTasks file tasks = writeFile file (show tasks)

--b
addTask :: String -> TaskList -> TaskList
addTask task tasks = tasks ++ [(False, task)]

--c
numberTasks :: TaskList -> [(Int, Task)]
numberTasks tasks = zipWith (\l r -> (r, l)) tasks [1 ..]

--d
markDone :: Int -> TaskList -> TaskList
markDone i tasks = markHelper (numberTasks tasks) i

markHelper :: [(Int, Task)] -> Int -> TaskList
markHelper [] _ = []
markHelper ((index, (b, task)) : xs) i
  | index == i = (True, task) : markHelper xs i
  | otherwise = (b, task) :markHelper xs i

--e TODO: Video 8:20  https://tube.switch.ch/videos/51c3dc65
main :: IO ()
main = do 
  (x:xs:xss) <- getArgs



