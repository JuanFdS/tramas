module Byte where

import Prelude

import Data.Array (dropEnd, length, replicate, takeEnd, take, drop, concat)
import Data.Foldable (foldl)
import Data.Int (rem)
import Data.NonEmpty ((:|))
import Test.QuickCheck.Gen (Gen, elements, vectorOf)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)
import Data.Char (toCharCode, fromCharCode)
import Data.String.CodeUnits (fromCharArray)

type Byte = Array Bit

type Nibble = Array Bit

type Bit = Int

bitZero :: Bit
bitZero = 0

bitOne :: Bit
bitOne = 1

pad :: forall a. a -> Int -> Array a -> Array a
pad filler minSize array = replicate (max 0 $ minSize - length array) filler <> array

byteZero :: Byte
byteZero = replicate 8 bitZero

intToBits :: Int -> Array Bit
intToBits 0 = [bitZero]
intToBits 1 = [bitOne]
intToBits n = intToBits (n `div` 2) <> [n `rem` 2]

bitsToInt :: Array Bit -> Int
bitsToInt = foldl (\acum bit -> bit + acum * 2) 0

bitsToBytes :: Array Bit -> Array Byte
bitsToBytes bits | length bits <= 8 = [pad bitZero 8 bits]
                 | otherwise = bitsToBytes (dropEnd 8 bits) <> [takeEnd 8 bits]

bytesToBits :: Array Byte -> Array Bit
bytesToBits = concat

intToByte :: Int -> Byte
intToByte = intToBits

byteToInt :: Byte -> Int
byteToInt = bitsToInt

intToBytes :: Int -> Int -> Array Byte
intToBytes minQuantityOfBytes = intToBits >>> bitsToBytes >>> pad byteZero minQuantityOfBytes

bytesToInt :: Array Byte -> Int
bytesToInt = foldl (\acum byte -> byteToInt byte + acum * 256) 0

nibbleToInt :: Byte -> Int
nibbleToInt = bitsToInt

class Bits a where
    bits :: a -> Array Bit

bytes :: forall a. Bits a => a -> Array Byte
bytes = bitsToBytes <<< bits

showByte :: Byte -> String
showByte aByte = fromCharArray [nibbleToChar (take 4 aByte), nibbleToChar (drop 4 aByte)]
    where nibbleToChar nibble = case nibbleToInt nibble of
             n | n <= 9   -> advanceCharTimes n '0'
             n            -> advanceCharTimes (n - 10) 'A'
          advanceCharTimes times char = unsafePartial fromJust $ fromCharCode $ toCharCode char + times

bitGen :: Gen Bit
bitGen = elements (bitZero :| [bitOne])

byteGen :: Gen Byte
byteGen = vectorOf 8 bitGen
