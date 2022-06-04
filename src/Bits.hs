module Bits where

import Numeric
import Data.Char
import Debug.Trace
import Text.ParserCombinators.Parsec
-- 
type Bit = Char
type FourBit = String
type EightBit = String
type Bytes = String
type Hex = String

parseBit :: Parser Bit
parseBit = try (char '1') <|> char '0'

parse4bits :: Parser FourBit
parse4bits = do
    b1 <- parseBit
    b2 <- parseBit
    b3 <- parseBit
    b4 <- parseBit
    return [b1, b2, b3, b4]

parse8bits :: Parser EightBit
parse8bits = do
    b1 <- parse4bits
    b2 <- parse4bits
    return (b1 ++ b2)

readWith :: Parser a -> String -> a
readWith f s =
    case parse f "" s of
        Left e -> error (show e)
        Right t -> t

read8bits :: Bytes -> (EightBit, Bytes)
read8bits s = (readWith parse8bits s, drop 8 s)

boolToChar :: Bool -> Bit
boolToChar b = if b then '1' else '0'

charToInt :: Char -> Int
charToInt c = case c of
    '1' -> 1
    '0' -> 0
    _ -> error "invalid binary" 

charToBool :: Bit -> Bool
charToBool c = case c of
    '1' -> True
    '0' -> False
    _ -> error "invalid binary"

-- readBin :: ReadS Int
-- readBin = readInt 2 (`elem` "01") digitToInt

-- readHex :: ReadS Int
-- readHex = Numeric.readHex

-- bin2dec :: Bytes -> Int
-- bin2dec b = fst $ head $ readBin b

-- bin2hex :: Bytes -> Int
-- bin2hex b = fst $ head $ Numeric.readHex b

myBin2dec :: Bytes -> Int
myBin2dec = foldl (\x y -> charToInt y + 2*x) 0

myDec2bin :: Int -> Bytes
myDec2bin n = reverse $ myDec2binR n

myDec2binR :: Int -> Bytes
myDec2binR 0 = ""
myDec2binR n = head (show (n `mod` 2)) : myDec2binR (n `div` 2)

zeroPadding :: Int -> Bytes -> Bytes
zeroPadding n s = ['0' | _ <- [0..(n-l-1)]] ++ s where l = length s

littleendian2int :: Bytes -> Int
littleendian2int [] = 0
littleendian2int bytes = 128 * littleendian2int rest + myBin2dec b where (b, rest) = read8bits bytes

bigendian2int :: Bytes -> Int
bigendian2int = myBin2dec

toLe :: Bytes -> Bytes
toLe [] = ""
toLe bytes = toLe rest ++ b where (b, rest) = read8bits bytes

int2littleendian :: Int -> Int -> Bytes
int2littleendian bytelimit n = toLe bits where bits = int2bigendian bytelimit n

int2bigendian :: Int -> Int -> Bytes
int2bigendian bytelimit n = zeroPadding numBits $ myDec2bin n where numBits = 8 * bytelimit
