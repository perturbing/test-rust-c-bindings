{-# LANGUAGE ForeignFunctionInterface #-}

module MyBindings (randomScalarList) where

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Fr (..), sizeFr)
import Control.Monad (forM_)
import Data.Traversable (forM)
import Data.Void (Void)
import Data.Word (Word8)
import Foreign (ForeignPtr)
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, newForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal (copyBytes)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import GHC.Arr (listArray)

-- Foreign function interface for the Rust function
foreign import ccall "random_scalar_list" c_random_scalar_list :: Ptr Void -> CSize -> IO ()

-- Helper function to view bytes of a foreign pointer
viewBytes :: Ptr a -> Int -> IO [Word8]
viewBytes ptr size = peekArray size (castPtr ptr :: Ptr Word8)

-- Function to handle a list of `Fr` values
randomScalarList :: [Fr] -> IO [Fr]
randomScalarList frList = do
    if null frList
        then return []
        else do
            let lengthFr = length frList
            -- Combine the raw pointers into a single list
            ptrList <- concatFr frList
            -- Ensure all pointers are valid
            withForeignPtr ptrList $ \ptr -> do
                -- Call the Rust function with the pointer and length
                let len = fromIntegral lengthFr :: CSize
                c_random_scalar_list ptr len

                -- Convert `Ptr Void` back to `ForeignPtr Void` without using `finalizerFree`
                resultFps <- newForeignPtr_ ptr

                -- Split the `ForeignPtr Void` into a list of `Fr`
                splitFr resultFps lengthFr

concatFr :: [Fr] -> IO (ForeignPtr Void)
concatFr frList = do
    -- Calculate total size needed
    let totalSize = sizeFr * length frList

    -- Allocate the final large ForeignPtr
    finalPtr <- mallocForeignPtrBytes totalSize

    -- Copy each Fr element into the finalPtr
    withForeignPtr finalPtr $ \destPtr -> do
        let copyElement offset (Fr srcPtr) = withForeignPtr srcPtr $ \src ->
                copyBytes (destPtr `plusPtr` offset) src sizeFr

        -- Use a forM_ loop to copy each element
        forM_ (zip [0, sizeFr ..] frList) $ uncurry copyElement

    -- Return the final ForeignPtr
    return finalPtr

-- Function to split a ForeignPtr containing multiple Fr elements into a list of individual Fr elements
splitFr :: ForeignPtr Void -> Int -> IO [Fr]
splitFr srcPtr n = do
    -- Iterate over the number of elements and extract each `Fr`
    withForeignPtr srcPtr $ \src ->
        forM [0 .. n - 1] $ \i -> do
            -- Allocate memory for each individual `Fr`
            frPtr <- mallocForeignPtrBytes sizeFr
            -- Copy the corresponding bytes from the original buffer
            withForeignPtr frPtr $ \dst ->
                copyBytes dst (src `plusPtr` (i * sizeFr)) sizeFr
            -- Return the new `Fr`
            return $ Fr frPtr
