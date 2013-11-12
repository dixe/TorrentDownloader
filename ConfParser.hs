-- this module parses the config file and returns the search string
-- the config file is lines with atributes title, season and episode, with :
-- attributes names end with : and are seperated with ,

module ConfParser(createSearchT, readConfig, writeConfig) where

import Data.List.Split (splitOn)
import ConfigData

test = DownData "Modern Family" 3 1

-- create a sertch term from a download fx modern famlily s01e02
createSearchT :: Download -> String
createSearchT (DownData t s e) = t ++ " s" ++ sToDigit s ++"e"++ sToDigit e
createSearchT Empty = error "Nothing to download"

-- create a String from int, with two digit, possible leading 0
sToDigit x = if x>9 then show x else "0" ++ show x

-- returns the config file in
readConfig :: IO [Download]
readConfig = do
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

-- given a filePath and list of Download, write a download pr line
writeConfig ::String-> [Download] -> IO ()
writeConfig location write = do
  let files = toFileString write
  writeFile location files

-- takes a download and returns a string to be places in config
downToString :: Download -> String
downToString (DownData title s e) = "Title: " ++ title ++ ", Season: " ++ show s ++ ", Episode: " ++ show e
downToString Empty = ""

toFileString :: [Download] -> String
toFileString = foldl(\acc x -> (downToString x) ++ "\n" ++acc) ""