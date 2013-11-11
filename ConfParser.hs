-- this module parses the config file and returns the search string
-- the config file is lines with atributes title, season and episode, with :
-- attributes names end with : and are seperated with ,

module ConfParser(createSearchT, parseConfig, Download) where

import Data.List.Split (splitOn)
import Control.Monad.IO.Class -- (liftIO)

-- download object with title,season,episode
data Download = DownData String Int Int | Empty deriving (Show)

test = DownData "how i met yout mother" 09 08

createSearchT :: Download -> String
createSearchT (DownData t s e) = t ++ " s" ++ sToDigit s ++"e"++ sToDigit e
createSearchT Empty = error "Nothing to download"

-- create a String from int, with two digit, possible leading 0
sToDigit x = if x>9 then show x else "0" ++ show x

parseConfig :: IO [Download]
parseConfig = do
  file <- readFile "config.Conf"
  return $ transformConf $ lines file

-- transform a list of unparsed episodes into a list of Download
-- use with the line file, where file i readFile config.Conf
transformConf :: [String] -> [Download]
transformConf xs = foldr(\x acc -> (convert (splitOn "," x)):acc)[] xs where
    convert :: [String] -> Download
    convert s = DownData (getTitle $ head s) (getSeason. head $ drop 1 s) (getEpisode. head $ drop 2 s)
    getTitle t = strip. head . tail $ splitOn "Title:" t
    getSeason s = read . strip.head.tail $  splitOn "Season:" s
    getEpisode e = read . strip.head.tail $  splitOn "Episode:" e

strip :: String -> String
strip (' ':s) = strip s
strip  s = s