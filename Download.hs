-- download the html body

module Download(downloadHtml, createSearchUrl) where

import Data.Char
import Network.HTTP
import System.Environment
import System.Process
import Control.Concurrent



-- Takes a url and save the body in html.txt
downloadHtml url =  simpleHTTP (getRequest ( formatUrl url)) >>=  getResponseBody


-- replaces space with %20
formatUrl = foldl (\acc x -> acc ++ (if isSpace x then "%20" else x:[] )) ""

-- given search term, creates the search url
createSearchUrl term = "http://tpbunblocked.me/search/"++ term ++ "/0/7/208"


url = "http://www.tpbunblocked.me"