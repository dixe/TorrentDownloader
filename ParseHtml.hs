module ParseHtml where
import Text.HTML.TagSoup

-- Findes number of first link, the magnet links is this number +6
findTagNum :: Num a => [Tag String] -> a -> a
findTagNum [] n = n
findTagNum (TagOpen "a" (("href",_):("class","detLink"):_):xs) n= n
findTagNum (x:xs) n = findTagNum xs (n+1)

-- gets the magnet link by parseing the file, and finding where the first link is
-- then add 6 to the number and get the magnet link from that Tag
-- returns the first magnet link in the html file
getMagnet = do
  file <- readFile "html.txt"
  return $ parseMagnet $ take 1 $ drop (6 + (findTagNum ( parseTags file) 0)) (parseTags file)

-- find the magnet link from
parseMagnet ((TagOpen "a" (("href",link):_)):xs) = link
parseMagnet [] = ""


