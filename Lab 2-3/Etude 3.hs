{-# LANGUAGE GADTs #-}
{-# LANGUAGE ParallelListComp #-}

import Control.Monad
import Data.List
import Data.Monoid
import Formatting
import Formatting.Formatters
import qualified Data.Text.Lazy as TL

average :: Fractional a => [a] -> a
average a = sum a / fromIntegral (length a)

sigma :: Floating a => [a] -> a
sigma list = sqrt $ sum (map ((^2) . (subtract $ average list)) list) / fromIntegral (length list * (length list - 1))

mergeSigmas :: Floating a => a -> a -> a
mergeSigmas sigma1 sigma2 = sqrt $ sigma1 ^ 2 + sigma2 ^ 2

powersProduct :: Floating a => [a] -> [a] -> a
powersProduct powers values = product (zipWith (**) values powers)

sigmaOfPowersProduct :: Floating a => [a] -> [a] -> [a] -> a
sigmaOfPowersProduct powers values sigmas = powersProduct powers values * (sqrt . sum $ map (**2) (zipWith (*) powers (zipWith (/) sigmas values)))

data ValueSigma a where
    ValueSigma :: RealFrac a => a -> a -> ValueSigma a -- why RealFrac?

instance Show (ValueSigma a) where
    show (ValueSigma value sigma) = fmt value <> " ± " <> fmt sigma where
        numberOfDigits = 2 -- the precision should be 10-20%, how can we achieve this?
        fmt = TL.unpack . format (fixed numberOfDigits)

main = do
    let parseInput = map (read :: String -> Double) . words
    systematicError <- putStrLn "systematic error: " >> (read :: String -> Double) <$> getLine
    measurements <- putStrLn "measurements: " >> replicateM 2 (parseInput <$> getLine)

    let averages = map average measurements
    let sigmas = map sigma measurements
    let sigmasFull = map (mergeSigmas systematicError) sigmas

    let mass = average averages
    let massSigma = foldl1 mergeSigmas sigmasFull

    let fmt = TL.unpack . format (fixed 2)
    putStr . unlines $ zipWith (<>) [x <> " плечо:\n" | x <- ["Левое", "Правое"]]
        (map (unlines . zipWith (<>) [x <> " = " | x <- ["m ср.", "σ", "σ полн."]] . map fmt) (transpose [averages, sigmas, sigmasFull]))
    putStrLn $ "m = " <> show (ValueSigma mass massSigma)

    [volumes, volumeSigmas] <- putStrLn "volume ± σ values: " >> (transpose <$> replicateM 2 (parseInput <$> getLine))
    
    let densities = [powersProduct [1, -1] [mass, volume] | volume <- volumes] 
    let densitySigmas = [sigmaOfPowersProduct [1, -1] [mass, volume] [massSigma, volumeSigma] | [volume, volumeSigma] <- transpose [volumes, volumeSigmas]]
    putStr . unlines $ zipWith (<>) [x <> ": " | x <- ["Линейка", "Штангенциркуль"]] 
        [show (ValueSigma density sigma) <> " г/см³" | [density, sigma] <- transpose [densities, densitySigmas]]
