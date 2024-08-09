{-# LANGUAGE ForeignFunctionInterface #-}

module MyBindings (doubleFr) where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Fr (..), sizeFr)
import Data.Void (Void)
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

foreign import ccall "random_scalar" c_random_scalar :: Ptr Void -> IO (Ptr Void)

viewBytes :: Ptr a -> Int -> IO [Word8]
viewBytes ptr size = peekArray size (castPtr ptr :: Ptr Word8)

doubleFr :: Fr -> IO Fr
doubleFr (Fr fp) = withForeignPtr fp $ \ptr -> do
    if ptr == nullPtr
        then putStrLn "Pointer is null before calling Rust function!"
        else putStrLn $ "Pointer is valid: " ++ show ptr

    originalValues <- viewBytes ptr sizeFr
    putStrLn $ "Original Values: " ++ show originalValues

    resultPtr <- c_random_scalar (castPtr ptr)
    if resultPtr == nullPtr
        then putStrLn "Received null pointer back from Rust function!"
        else putStrLn $ "Result Pointer is valid: " ++ show resultPtr

    resultValues <- viewBytes (castPtr resultPtr) sizeFr
    putStrLn $ "Result Values: " ++ show resultValues

    resultFp <- newForeignPtr finalizerFree (castPtr resultPtr :: Ptr Void)
    return (Fr (castForeignPtr resultFp))
