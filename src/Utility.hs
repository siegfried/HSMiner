module Utility where

import Data.Word (Word8, Word32)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Char (intToDigit, digitToInt)

toWord32 :: [Word8] -> Word32
toWord32 (a:b:c:d:[]) = a' .|. b' .|. c' .|. d' where
    a' = shiftL (fromIntegral a) 24
    b' = shiftL (fromIntegral b) 16
    c' = shiftL (fromIntegral c) 8
    d' = fromIntegral d

toWord8 :: Word32 -> [Word8]
toWord8 x = [a, b, c, d] where
    a = fromIntegral $ shiftR x 24
    b = fromIntegral $ shiftR x 16
    c = fromIntegral $ shiftR x 8
    d = fromIntegral x

toHex :: String -> [Word8]
toHex []       = []
toHex (h:l:xs) = word : toHex xs where
    word = fromIntegral $ shiftL (digitToInt h) 8 .|. digitToInt l

toStr :: [Word8] -> String
toStr = foldr str []

str :: Word8 -> String -> String
str x ys = [hChar x, lChar x] ++ ys where
    hChar = intToDigit . fromIntegral . (`div` 0x10)
    lChar = intToDigit . fromIntegral . (0x0f .&.)

makeRawBlockHeader :: String -> String -> String -> String -> String -> String -> [Word8]
makeRawBlockHeader v ph mr tp tt n = toHex $ v ++ ph ++ mr ++ tp ++ tt ++ n

