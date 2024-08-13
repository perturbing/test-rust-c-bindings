module Main where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Scalar (..), frFromScalar, scalarFromFr, scalarFromInteger, scalarToInteger)
import MyBindings (doubleFr)

main :: IO ()
main = do
    scalar <- scalarFromInteger 44088922575230112513242719452924342913839627358734666446284982958943915819480
    fr <- frFromScalar scalar
    resultFr <- doubleFr fr
    resultScalar <- scalarFromFr resultFr
    y <- scalarToInteger resultScalar
    putStrLn $ "The random scalar via the C binding is: " ++ show y
