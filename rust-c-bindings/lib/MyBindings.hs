{-# LANGUAGE ForeignFunctionInterface #-}

module MyBindings (doubleFr) where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Fr (..), sizeFr)
import Data.Void (Void)
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr)

foreign import ccall "random_scalar" c_random_scalar :: Ptr Void -> IO (Ptr Void)

viewBytes :: Ptr a -> Int -> IO [Word8]
viewBytes ptr size = peekArray size (castPtr ptr :: Ptr Word8)

doubleFr :: Fr -> IO Fr
doubleFr (Fr fp) = withForeignPtr fp $ \ptr -> do
    putStrLn $ "Original Pointer: " ++ show ptr
    originalValues <- viewBytes ptr sizeFr
    putStrLn $ "Original Values: " ++ show originalValues

    resultPtr <- c_random_scalar ptr
    putStrLn $ "Result Pointer: " ++ show resultPtr
    resultValues <- viewBytes (castPtr resultPtr) sizeFr
    putStrLn $ "Result Values: " ++ show resultValues

    resultFp <- newForeignPtr finalizerFree (castPtr resultPtr :: Ptr Void)
    putStrLn $ "Result ForeignPtr: " ++ show resultFp
    return (Fr (castForeignPtr resultFp))
