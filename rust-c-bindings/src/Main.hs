module Main where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Scalar (..), frFromScalar, scalarFromFr, scalarFromInteger, scalarToInteger)
import MyBindings (doubleFr)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    scalar <- scalarFromInteger 1
    fr <- frFromScalar scalar
    resultFr <- doubleFr fr
    resultScalar <- scalarFromFr resultFr
    y <- scalarToInteger resultScalar
    putStrLn $ "The random scalar via the C binding is: " ++ show y
