{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Safe where

-- perhaps change the ccall convention to something else?

import Foreign.C.Types

fib :: Int -> Int
fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib_hs :: CInt -> CInt
fib_hs = fromIntegral . fib . fromIntegral

foreign export ccall fib_hs :: CInt -> CInt

