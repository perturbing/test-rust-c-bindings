module Main where

import MyBindings (addViaC)
import System.IO (hFlush, stdout)

-- Function to prompt the user for an integer
promptForInt :: String -> IO Int
promptForInt prompt =
    do
        putStr prompt
        hFlush stdout
        read <$> getLine

main :: IO ()
main = do
    -- Prompt the user for two integers
    putStrLn "Hello, Haskell!"
    x <- promptForInt "Enter the first integer: "
    y <- promptForInt "Enter the second integer: "

    -- Call the addViaC function
    let result = addViaC x y

    -- Print the result
    putStrLn $ "The result of adding " ++ show x ++ " and " ++ show y ++ " via the C binding is: " ++ show result
