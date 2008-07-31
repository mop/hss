-- This module prettifies an HTML string by filtering all evil tags and
-- formatting it to plaintext
module StringPrettifier (
    prettify
) 
where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJ
import Data.List(isPrefixOf)
import Data.Char(isSpace)


-- Prettifies the given string so that the string doesn't run's over the given
-- width.
prettify :: Int -> String -> String
prettify width "" = ""
prettify width str | length str <= width = str
                   | otherwise = splitStr width str

-- Filters _all_ HTML Tags
filterHtmlTags :: String -> String
filterHtmlTags str = doFilter False str
    where   doFilter True ('>':xs) = doFilter False (xs)
            doFilter False ('<':xs) = doFilter True (xs)
            doFilter True (x:xs) = doFilter True (xs)
            doFilter False (x:xs) = x : doFilter False (xs)
            doFilter _ [] = []

-- does the actual work
-- splitted: is a [[String]]-List. The inner List represents each word and the
-- outher list each paragraph. 
splitStr :: Int -> String -> String
splitStr width str = renderStyle Style { lineLength = width, mode=PageMode, ribbonsPerLine = 1.5 } $ vcat innerDocs
    where   innerDocs = map (\xs -> fsep (map text xs)) splitted
            splitted = map (\x -> checkWords (split x ' ')) 
                         $ map (\x -> (trim x) ++ "\n" ) (split replaced '\n')
            checkWords [] = []
            checkWords (x:xs) | length x > width = fst (splitAt width x) : 
                checkWords (snd (splitAt width x) : xs)
                              | otherwise        = x : checkWords xs
            replaced = filterHtmlTags $ foldr 
                (\pat str -> replace str pat "\n") str replaceList
            replaceList = [ 
                "<br />", "<br/>", "<br>", "<BR>", "<BR/>", "<BR />" ]

-- Splits a list
split :: String -> Char -> [ String ]
split string char = doSplit string "" 0
	where 	doSplit :: String -> String -> Integer -> [ String ]
		doSplit [] splitted pos 
			| splitted /= "" = splitted : []
			| otherwise 	 = []
		doSplit (c:cs) splitted pos 
			| c == char = splitted : doSplit cs "" (pos + 1)
			| otherwise = doSplit cs (splitted ++ [c]) (pos + 1) 

-- Flattens a list
flatten :: [[a]] -> [a]
flatten = foldl (++) []

-- Replaces the given array with where the given pattern matches and returns
-- the replaced string
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] what repl = []
replace str@(x:xs) what repl 
    | what `isPrefixOf` str = repl ++ (replace replaced what repl)
    | otherwise = x : replace xs what repl
    where  replaced = drop (length what) str

trim      :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace
