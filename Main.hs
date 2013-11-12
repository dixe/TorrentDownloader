-- download the html body and starts the torrent file

module Main where

import System.Environment
import System.Process
import Download
import ParseHtml
import ConfParser
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Exception
import Control.Exception.Base
import System.IO.Error
import System.Exit

--takes a magnet link, start downloading the torrent via utorrent
startMagnet magnet = if magnet == ""
                     then Nothing
                     else Just (sMagnet magnet)

sMagnet magnet = do
  let torrentCommand = "utorrent \"" ++ magnet ++ "\""
  runCommand torrentCommand -- starts utorrent and download the file in magnet url

-- can be change to later to use commandline flags for different behavior
main = do
  downloadT -- download the torrent specified in the config.conf file

-- this as a hacked way of making things run
runEmpty = do
  runCommand ""



getUrls :: [Download] -> [String]
getUrls (d:ds) = (createSearchUrl $ createSearchT d) : getUrls ds
getUrls [] = []


-- function to download torrent
downloadT = do
  downloads <- readConfig
  let urls = getUrls downloads
  startTorrent urls

startTorrent (url:us) = do
  downloadHtml url
  magnet <- getMagnet
  let runMagnet (Just x) = x
      runMagnet Nothing = runEmpty
  exitCode <- runMagnet (startMagnet magnet)
  startTorrent us
startTorrent [] = do
  print "Done"