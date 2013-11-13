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
import ConfigData
import ConfigChanger

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


rs = do
  handle <- runCommand ""
  getProcessExitCode handle


getUrls :: [Download] -> [String]
getUrls (d:ds) = (createSearchUrl $ createSearchT d) : getUrls ds
getUrls [] = []


-- function to download torrent
downloadT = do
  downloads <- readConfig
  let urls = getUrls downloads --create the urls to download
  m <-startTorrent urls [] -- get the magnet link and starts the torrent
  let magnets =  reverse m -- now it is the samme as in the config
  let upDatedDown = add1Episodes downloads magnets
  writeConfig "config.conf" upDatedDown

--startTorrnet :: [[Char]] -> [[Char]] -> IO [[Char]]
startTorrent (url:us) mags= do
  html <-  downloadHtml url
  let magnet = getMagnet html
  let runMagnet (Just x) = do x
                              return True
      runMagnet Nothing = do
                          return False
  mag <- runMagnet (startMagnet magnet)
  startTorrent us (mag:mags)
startTorrent [] mags= do
  return mags


test = [DownData "" 1 1]