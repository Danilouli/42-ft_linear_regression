-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: danilouli <danilouli@student.42.fr>        +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2020/01/30 18:42:41 by danilouli         #+#    #+#             --
--   Updated: 2020/03/05 19:29:32 by danilouli        ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

import Text.CSV
import Text.Read
import Debug.Trace
import Data.Function.Memoize

parseData :: [Record] -> [[Float]]
parseData [] = []
parseData xs = map parseCouple xs

toFloat :: String -> Float
toFloat = read
parseCouple [a,b] = [toFloat a, toFloat b]

splitData :: [[Float]] -> ([Float],[Float])
splitData xs = (map (\xxs -> xxs!!0) xs, map (\xxs -> xxs!!1) xs) 

mean :: [Float] -> Float
mean xs = (sum xs)/(fromIntegral(length xs))

toMult :: ([Float], [Float]) -> [Float]
toMult (xs,ys) = map (\(x,y)->x*y) (zip xs ys)

toSquare :: [Float] -> [Float]
toSquare xs = toMult (xs,xs)

covariance :: ([Float], [Float]) -> Float
covariance (xs,ys) = sum [(x - x_)*(y - y_) | x <- xs, y <- ys]
    where x_ = mean xs
          y_ = mean ys

variance :: [Float] -> Float
variance xs = covariance(xs,xs)

correlation :: ([Float], [Float]) -> Float
correlation (xs,ys) = covariance(xs,ys)/variance(xs)

interecept :: ([Float], [Float]) -> Float
interecept (xs,ys) = (mean ys) - (correlation(xs,ys) * (mean xs))

thetaCouple :: ([Float], [Float]) -> (Float,Float)
thetaCouple (xs,ys) = (interecept(xs,ys), correlation(xs,ys))

varianceKH :: [Float] -> Float
varianceKH xs = (mean (toSquare xs)) - (mean xs)*(mean xs)

covarianceKH :: ([Float], [Float]) -> Float
covarianceKH (xs,ys) = (mean (toMult(xs,ys))) - (mean xs)*(mean ys)

correlationKH :: ([Float], [Float]) -> Float
correlationKH (xs,ys) = covarianceKH(xs,ys)/varianceKH(xs)

intereceptKH :: ([Float], [Float]) -> Float
intereceptKH (xs,ys) = (mean ys) - (correlationKH(xs,ys) * (mean xs))

thetaCoupleKH :: ([Float], [Float]) -> (Float,Float)
thetaCoupleKH (xs,ys) = (intereceptKH(xs,ys), correlationKH(xs,ys))

allCouples :: ([Float], [Float]) -> [(Float,Float)]
allCouples (xs,ys) = [thetaCoupleKH(xs,ys), thetaCouple(xs,ys)]

handleError csv = putStrLn "error parsing"
doWork csv = (print.allCouples.splitData.parseData.tail) csv

main :: IO ()
main = do
    let fileName = "data.csv"
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv