module ConfigChanger(changeSeason, changeEpisode,add1Episodes) where

import ConfigData

-- change the season to a new season
changeSeason :: Download -> Int -> Download
changeSeason (DownData title s e) ns = DownData title ns e
changeSeason Empty _ = Empty

changeEpisode :: Download -> Int -> Download
changeEpisode (DownData title s e) ne = DownData title s ne
changeEpisode Empty _ = Empty

-- increment season with 1
add1Season :: Download -> Download
add1Season (DownData title s e) = DownData title (s+1) e
add1Season Empty = Empty

-- increment episode with 1
add1E :: Download -> Download
add1E (DownData title s e) = DownData title s (e+1)
add1E Empty = Empty

-- add 1 to episodes, where magnet is not ""
add1Episodes :: [Download] -> [Bool] -> [Download]
add1Episodes d m = reverse (add1Episodes' d m)

add1Episodes' :: [Download] -> [Bool] -> [Download]
add1Episodes' (d:ds) (False:ms) = (d):(add1Episodes' ds ms)
add1Episodes' (d:ds) (True:ms) = (add1E d):(add1Episodes' ds ms)
add1Episodes' [] _ = []
add1Episodes' ds [] = ds