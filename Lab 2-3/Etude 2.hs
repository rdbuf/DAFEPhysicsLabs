{-# LANGUAGE GADTs #-}

import Data.Monoid
import Formatting
import Formatting.Formatters
import qualified Data.Text.Lazy as TL
import Data.List (intersperse)
import Control.Monad

average :: Fractional a => [a] -> a
average a = sum a / fromIntegral (length a)

sigma :: Floating a => [a] -> a
sigma list = sqrt $ sum (map ((^2) . (subtract $ average list)) list) / fromIntegral (length list * (length list - 1))

mergeSigmas :: Floating a => a -> a -> a
mergeSigmas sigma1 sigma2 = sqrt $ sigma1 ^ 2 + sigma2 ^ 2

powersProduct :: Floating a => [a] -> [a] -> a
powersProduct powers values = product (zipWith (**) values powers)

sigmaOfPowersProduct :: Floating a => [a] -> [a] -> [a] -> a
sigmaOfPowersProduct powers values sigmas = powersProduct powers values * (sqrt . sum $ zipWith (*) (map (^2) powers) (zipWith (/) sigmas (map (^2) values)))

data ValueSigma a where
    ValueSigma :: RealFrac a => a -> a -> ValueSigma a -- why RealFrac?

instance Show (ValueSigma a) where
    show (ValueSigma value sigma) = fmt value <> " ± " <> fmt sigma where
        numberOfDigits = 2 -- the precision should be 10-20%, how can we achieve this?
        fmt = TL.unpack . format (fixed numberOfDigits)

main = do
    let parseInput = map (read :: String -> Double) . words
    systematicError <- putStrLn "systematic error: " >> (read :: String -> Double) <$> getLine
    measurements <- putStrLn "measurements: " >> (sequence $ replicate 3 (parseInput <$> getLine))

    let averages = map average measurements
    let sigmas = map sigma measurements
    let sigmasFull = map (mergeSigmas systematicError) (drop 1 sigmas) <> [last sigmas]

    let volume = powersProduct [1,2,1] averages
    let volumeSigma = sigmaOfPowersProduct [1,2,1] averages sigmasFull

    let fmt = join . intersperse ", " . map (TL.unpack . format (fixed 3))
    putStrLn ("averages: " <> fmt averages)
    putStrLn ("sigmas: " <> fmt sigmas)
    putStrLn ("sigmasFull: " <> fmt sigmasFull)

    putStrLn ("volume: " <> fmt [volume])
    putStrLn ("volumeSigma: " <> fmt [volumeSigma])

    putStrLn "---------------"

    putStr . unlines $ zipWith (<>) [x <> " = " | x <- ["a", "b", "const"]] (map show $ zipWith ValueSigma averages sigmasFull)
    putStrLn $ "volume = " <> show (ValueSigma volume volumeSigma)