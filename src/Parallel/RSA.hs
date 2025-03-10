--
-- Derived from a program believed to be originally written by John
-- Launchbury, and incorporating the RSA algorithm which is in the
-- public domain.
--

module Parallel.RSA where

import Control.Parallel.Strategies (parBuffer, parList, rdeepseq, withStrategy)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B
import System.Environment

main :: IO ()
main = do
    [cmd, f] <- getArgs
    text <- case f of
        "-" -> B.getContents
        _ -> B.readFile f
    case cmd of
        "encrypt" -> B.putStr (encrypt n e text)
        "encryptStrat" -> B.putStr (encryptStrat n e text)
        "encryptStratBad" -> B.putStr (encryptStratBad n e text)
        "decrypt" -> B.putStr (decrypt n d text)
        _ -> error $ "Invalid command: " <> cmd

-- example keys, created by makeKey below
n, d, e :: Integer
(n, d, e) = (3539517541822645630044332546732747854710141643130106075585179940882036712515975698104695392573887034788933523673604280427152984392565826058380509963039612419361429882234327760449752708861159361414595229, 121492527803044541056704751360974487724009957507650761043424679483464778334890045929773805597614290949, 216244483337223224019000724904989828660716358310562600433314577442746058361727768326718965949745599136958260211917551718034992348233259083876505235987999070191048638795502931877693189179113255689722281)

encrypt, encryptStrat, decrypt :: Integer -> Integer -> ByteString -> ByteString
-- <<encrypt
encrypt n e =
    B.unlines -- <3>
        . map (B.pack . show . power e n . code) -- <2>
        . chunk (size n) -- <1>
        -- >>
encryptStrat n e =
    B.unlines -- <3>
        . withStrategy (parBuffer 100 rdeepseq)
        . map (B.pack . show . power e n . code) -- <2>
        . chunk (size n) -- <1>
        -- >>

encryptStratBad n e =
    B.unlines -- <3>
        . withStrategy (parList rdeepseq)
        . map (B.pack . show . power e n . code) -- <2>
        . chunk (size n) -- <1>
        -- >>

decrypt n d =
    B.concat
        . map (B.pack . decode . power d n)
        . integers
        . B.lines

integers :: [ByteString] -> [Integer]
integers bs = [i | Just (i, _) <- map B.readInteger bs]

-------- Converting between Strings and Integers -----------

code :: ByteString -> Integer
code = B.foldl' accum 0
  where
    accum x y = (128 * x) + fromIntegral (fromEnum y)

decode :: Integer -> String
decode n = reverse (expand n)
  where
    expand 0 = []
    expand x = toEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)

chunk :: Int -> ByteString -> [ByteString]
chunk _ xs | B.null xs = []
chunk n xs = as : chunk n bs
  where
    (as, bs) = B.splitAt (fromIntegral n) xs

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100 -- log_128 10 = 0.4745

------- Constructing keys -------------------------

makeKeys :: Integer -> Integer -> (Integer, Integer, Integer)
makeKeys r s = (p * q, d, invert ((p - 1) * (q - 1)) d)
  where
    p = nextPrime r
    q = nextPrime s
    d = nextPrime (p + q + 1)

nextPrime :: Integer -> Integer
nextPrime a = head (filter prime [odd, odd + 2 ..])
  where
    odd
        | even a = a + 1
        | otherwise = a
    prime p = and [power (p - 1) p x == 1 | x <- [3, 5, 7]]

invert :: Integer -> Integer -> Integer
invert n a = if e < 0 then e + n else e
  where
    e = iter n 0 a 1

iter :: Integer -> Integer -> Integer -> Integer -> Integer
iter g v 0 w = v
iter g v h w = iter h w (g `mod` h) (v - (g `div` h) * w)

------- Fast exponentiation, mod m -----------------

power :: Integer -> Integer -> Integer -> Integer
power 0 _ _ = 1
power n m x
    | even n = sqr (power (n `div` 2) m x) `mod` m
    | otherwise = (x * power (n - 1) m x) `mod` m

sqr :: Integer -> Integer
sqr x = x * x
