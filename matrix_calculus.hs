-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   matrix_calculus.hs                                 :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: danilouli <danilouli@student.42.fr>        +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2020/01/30 18:42:41 by danilouli         #+#    #+#             --
--   Updated: 2020/02/01 13:07:27 by danilouli        ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

import Data.Matrix
import Text.CSV
import Text.Read

m_generator :: (Float, Float) -> Float
m_generator (1,1) = 0
m_generator (i,j) = 2*i - 3*j

parseData :: [Record] -> [[Float]]
parseData [] = []
parseData xs = map parseCouple xs

toFloat :: String -> Float                               
toFloat = read
parseCouple [a,b] = [toFloat a, toFloat b]
                               
splitData xs = (map (\xxs -> xxs!!0) xs, map (\xxs -> xxs!!1) xs) 

matrixialize :: [Float] -> Matrix Float
matrixialize xs = (matrix (length xs) 1 $ (\(i,j) -> xs!!(i-1)))

doubleMatrixialize :: ([Float],[Float]) -> (Matrix Float, Matrix Float)
doubleMatrixialize (xs,ys) = (matrixialize xs, matrixialize ys)

inverseFallback :: (Either String (Matrix Float)) -> Matrix Float
inverseFallback minv = either (\m -> unitMatrix) (\m -> m) minv
    where unitMatrix = matrix 1 1 (\(i,j) -> 1)

minimize :: (Matrix Float, Matrix Float) -> Matrix Float
minimize (x,y) = (inverseFallback(inverse(transpose(x)*x)))*(transpose(x))*y

mean :: [Float] -> Float
mean xs = (sum xs)/(fromIntegral(length xs))

covariance :: ([Float], [Float]) -> Float
covariance (xs,ys) = sum [(x - x_)*(y - y_) | x <- xs, y <- ys]
    where x_ = mean xs
          y_ = mean ys

variance :: [Float] -> Float
variance xs = covariance(xs,xs)

correlation :: ([Float], [Float]) -> Float
correlation (xs,ys) = covariance(xs,ys)/variance(xs)

handleError csv = putStrLn "error parsing"
doWork csv = (print.correlation.splitData.parseData.tail) csv

main :: IO ()
main = do
    let fileName = "data.csv"
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv