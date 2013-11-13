-- download the html body

module Download(downloadHtml, createSearchUrl) where

import Data.Char
import System.Environment
import System.Process
import Control.Concurrent



-- Takes a url and save the body in html.txt
downloadHtml url = do
  let htmlCommand = "python pyGet.py " ++ (formatUrl url)
  handle <- runCommand htmlCommand -- save the html to html.txt
  waitForProcess handle




-- replaces space with %20
formatUrl = foldl (\acc x -> acc ++ (if isSpace x then "%20" else x:[] )) ""

-- given search term, creates the search url
createSearchUrl term = "http://thepiratebay.sx/search/"++ term ++ "/0/7/208"


url = "http://www.thepiratebay.sx"