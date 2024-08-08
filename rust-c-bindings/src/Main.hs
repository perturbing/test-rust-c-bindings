module Main where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Scalar (..), frFromScalar, scalarFromFr, scalarFromInteger, scalarToInteger)
import MyBindings (doubleFr)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    scalar <- scalarFromInteger 27125923900743449446737244713810001155895196669447159259758142105625740449839
    fr <- frFromScalar scalar
    resultFr <- doubleFr fr
    resultScalar <- scalarFromFr resultFr
    y <- scalarToInteger resultScalar
    putStrLn $ "The random scalar via the C binding is: " ++ show y
