-- Mini Calculator implemetation from Worksheet WS_MiniCalc
main = do
  putStrLn "Welcome to MiniCalc!"
  putStrLn "Please enter you first Number"
  s1 <- getLine
  putStrLn "Please enter your second Number"
  s2 <- getLine
  putStrLn "Please enter your desired operation"
  s3 <- getLine
  -- Parse String to Int
  let n1 = read s1 :: Int
  let n2 = read s2 :: Int
  let sign = s3
  -- Evaluate the desired Operation and calcualte the corresponding result with it.
  let result
        | sign == "+" = n1 + n2
        | sign == "-" = n1 - n2
        | sign == "/" = n1 `div` n2
        | sign == "*" = n1 * n2
        | otherwise = error "Wrong Operation"
  -- Print Result to Console
  putStrLn (show (n1) ++ sign ++ show (n2) ++ " = " ++ show result)
  main
