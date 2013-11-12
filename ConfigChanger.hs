module ConfigChanger(changeSeason, changeEpisode,add1Season,
add1Episode) where

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
add1Episode :: Download -> Download
add1Episode (DownData title s e) = DownData title (s+1) e
add1Episode Empty = Empty