-- download the html body and starts the torrent file

module Main where

import System.Environment
import System.Process
import Download
import ParseHtml
import ConfParser
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

-- takes a magnet link, start downloading the torrent via utorrent
startMagnet  magnet = if magnet /=""
                      then sMagnet magnet
                      else error "empty magnet"

sMagnet magnet = do
  let torrentCommand = "utorrent \"" ++ magnet ++ "\""
  handle <- runCommand torrentCommand -- starts utorrent and download the file in magnet url
  waitForProcess handle

--tmp
shUrl = "http://thepiratebay.sx" ++ "/search/how i met your mother s09e03/0/7/208"

--
main = do
  downloads <- parseConfig
  let urls = getUrls downloads
  download urls



getUrls :: [Download] -> [String]
getUrls (d:ds) = (createSearchUrl $ createSearchT d) : getUrls ds
getUrls [] = []

download (url:us) = do
  downloadHtml url
  magnet <- getMagnet
  startMagnet magnet
  download us

download [] = do
  print "Done"