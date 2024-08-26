module Main where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Scalar (..), frFromScalar, scalarFromFr, scalarFromInteger, scalarToInteger)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.IO (unsafePerformIO)
import qualified Hedgehog.Internal.Gen as G
import qualified Hedgehog.Internal.Range as R
import MyBindings (randomScalarList)

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

listOfSizedIntegers :: Integer -> Integer -> [Integer]
listOfSizedIntegers n l =
    unsafePerformIO
        . G.sample
        $ ( map byteStringToInteger
                <$> G.list
                    (R.singleton $ fromIntegral n)
                    (G.bytes (R.singleton $ fromIntegral l))
          )

main :: IO ()
main = do
    -- A list of the integer 0 to 100
    -- let integers = [0..10000000]
    -- Generate a list random integers with 31 bytes each
    let integers = listOfSizedIntegers 1000 31

    -- Convert the integers to Scalars
    scalars <- mapM scalarFromInteger integers

    -- Convert the Scalars to Fr values
    frs <- mapM frFromScalar scalars

    -- Use randomScalarList on list of values
    resultFrs <- randomScalarList frs
    let lastElement = last resultFrs
    lastScalar <- scalarFromFr lastElement
    lastInteger <- scalarToInteger lastScalar
    print lastInteger
