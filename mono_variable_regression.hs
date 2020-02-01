-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   mono_variable_regression.hs                        :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: danilouli <danilouli@student.42.fr>        +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2020/01/30 18:42:41 by danilouli         #+#    #+#             --
--   Updated: 2020/02/01 13:58:05 by danilouli        ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

import Text.CSV
import Text.Read

parseData :: [Record] -> [[Float]]
parseData [] = []
parseData xs = map parseCouple xs

toFloat :: String -> Float                               
toFloat = read

parseCouple :: [String] -> [Float]
parseCouple [a,b] = [toFloat a, toFloat b]

splitData :: [[a]] -> ([a],[a])                               
splitData xs = (map (\xxs -> xxs!!0) xs, map (\xxs -> xxs!!1) xs) 

mean :: [Float] -> Float
mean xs = (sum xs)/(fromIntegral(length xs))

covariance :: ([Float], [Float]) -> Float
covariance (xs,ys) = (sum [(x - x_)*(y - y_) | x <- xs, y <- ys])/dlen
    where x_ = mean xs
          y_ = mean ys
          dlen = (fromIntegral (length xs))*(fromIntegral (length ys))

variance :: [Float] -> Float
variance xs = covariance(xs,xs)

correlation :: ([Float], [Float]) -> Float
correlation (xs,ys) = covariance(xs,ys)/variance(xs)

origin :: ([Float], [Float]) -> Float
origin (xs,ys) = (mean ys) - correlation(xs,ys)*(mean xs)

thetaCouple :: ([Float], [Float]) -> (Float, Float)
thetaCouple (xs, ys) = (correlation(xs,ys), origin(xs,ys))

handleError csv = putStrLn "error parsing"
doWork csv = (print.thetaCouple.splitData.parseData.tail) csv

main :: IO ()
main = do
    let fileName = "data.csv"
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv