doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

twoQuestions :: IO ()
twoQuestions = do
  putStr "What is your name? "
  name <- getLine
  putStr "How old are you? "
  age <- getLine
  print (name, age)

twoQuestionsb :: IO ()
twoQuestionsb = putStr "What is your name? " >> getLine >>= \name -> putStr "How old are you? " >> getLine >>= \age -> print (name, age)

echo3 :: IO ()
echo3 = do
  l1 <- getLine
  l2 <- getLine
  putStrLn $ l1 ++ l2

{- Notes:

1. Definicja >>=
echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

2.
-}
