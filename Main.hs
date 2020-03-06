-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: danilouli <danilouli@student.42.fr>        +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2020/01/30 18:42:41 by danilouli         #+#    #+#             --
--   Updated: 2020/03/06 16:29:07 by danilouli        ###   ########.fr       --
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

scaleX :: [Float] -> [Float]
scaleX xs = map (\x -> (x - minimum xs)/((maximum xs) - (minimum xs))) xs

mt0 :: ([(Float,Float)], Float) -> (Int -> Float)
mt0(z,m) = (map (t0(z,m)) [0 ..] !!)
    where   t0(z,m) 0 = 0
            t0(z,m) n = t0 - (1/m)*scont
                where   t0 = (mt0(z,m)) (n-1)
                        t1 = (mt1(z,m)) (n-1)
                        scont = sum (map (\(x,y)->(t0 + t1*x - y)) z)

mt1 :: ([(Float,Float)], Float) -> (Int -> Float)
mt1(z,m) = (map (t1(z,m)) [0 ..] !!)
    where   t1(z,m) 0 = 0
            t1(z,m) n = t1 - (1/m)*scont
                where   t0 = (mt0(z,m)) (n-1)
                        t1 = (mt1(z,m)) (n-1)
                        scont = sum(map (\(x,y)->((t0 + t1*x - y)*x)) z)

uglyCouple :: ([Float], [Float]) -> (Float, Float)
uglyCouple (xs,ys) = ((mt1f 0)/((maximum xs) - (minimum xs)), mt0f 30)
    where m = fromIntegral(length xs)
          zipped = zip (scaleX xs) ys
          mt1f = mt1(zipped, m)
          mt0f = memoize (mt0(zipped, m))

allCouples :: ([Float], [Float]) -> [(Float,Float)]
allCouples (xs,ys) = [thetaCoupleKH(xs,ys), thetaCouple(xs,ys), uglyCouple(xs,ys)]

handleError csv = putStrLn "error parsing"
doWork csv = (print.allCouples.splitData.parseData.tail) csv

main :: IO ()
main = do
    let fileName = "data.csv"
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv