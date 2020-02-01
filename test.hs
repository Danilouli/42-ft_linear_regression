-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   test.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: danilouli <danilouli@student.42.fr>        +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2020/01/30 18:28:14 by danilouli         #+#    #+#             --
--   Updated: 2020/01/30 18:35:34 by danilouli        ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
import Data.Char
-- import Data.Aeson
doubleIt x = if x > 100 then x else x*2
toUpperCase s = map toUpper s
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)
     | x `elem` xs = rmdups xs
     | otherwise = x : rmdups xs
hasOne :: (a -> Bool) -> [a] -> Bool
hasOne _ [] = False
hasOne p (x:xs)
     | p x = True
     | otherwise = hasOne p xs
data Shape = 
    Circle {x :: Float, y :: Float, radius :: Float} |
    Rectangle {x1 :: Float, y1 :: Float, x2 :: Float, y2 :: Float}
surface :: Shape -> Float
surface (Circle {x,y,radius}) = 3.146 * radius ^ 2  
surface (Rectangle {x1,y1,x2,y2}) = (abs $ x2 - x1) * (abs $ y2 - y1) 