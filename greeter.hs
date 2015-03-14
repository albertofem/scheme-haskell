module Main where
import System.Environment
 
main :: IO ()
main = do
	args <- getArgs
	first <- readParameter
	second <- readParameter
	putStrLn ("Hello, " ++ first ++ " " ++ second)

readParameter :: IO String
readParameter = do
	putStrLn "Enter parameter: "
	getLine
