{-# LANGUAGE ForeignFunctionInterface #-}

module MyBindings (addViaC) where

import Foreign.C.Types

-- Import the Rust function
foreign import ccall "add" c_add :: CInt -> CInt -> CInt

-- Haskell wrapper function
addViaC :: Int -> Int -> Int
addViaC x y = fromIntegral (c_add (fromIntegral x) (fromIntegral y))
