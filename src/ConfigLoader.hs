module ConfigLoader (
     loadFeeds
    ,fetchFeeds
    ,writeFeeds
    ,mergeFeeds
)
where

import qualified Rss
import FeedParser
import HTTPFetcher
import Control.Monad
import qualified Control.Exception as Ex

-- The file with the string of feeds
feedListFile :: String
feedListFile = "feed_list"

-- The file with the list of feeds
feedFile :: String
feedFile = "feeds"

-- Merges two list of feeds. The first param is the cached version of the feeds
-- and the second param is the newly fetched version of the feeds. It marks the
-- read items in the newly fetched feed as read and returns it
mergeFeeds :: [Rss.RssFeed] -> [Rss.RssFeed] -> [Rss.RssFeed]
mergeFeeds readFeeds newFeeds = foldr replaceFeeds [] newFeeds
    where   replaceFeeds x ys = if length oldItems == 0 
                                    then x : ys
                                    else (head oldItems) : ys
                where   oldItems    = filter sameTitle readFeeds
                        sameTitle y = Rss.name x == Rss.name y

-- Loads the cached Rss-Feeds from the harddisk
loadFeeds :: IO [Rss.RssFeed]
loadFeeds = do
    file <- readFile feedFile 
    return $ read file
    `Ex.catch` (\x -> return ([]))

-- Fetches the feeds from the internet from the given feed-file
fetchFeeds :: IO [Rss.RssFeed]
fetchFeeds = do
    contents <- readFile feedListFile
    mapM fetchAndParse (lines contents)
    where   fetchAndParse line = (liftM myParse $ fetchFeed line)
                where   myParse content = (parseFeed content) { Rss.url = line }

-- Writes the RSS-files 
writeFeeds :: [Rss.RssFeed] -> IO ()
writeFeeds feeds = do 
    writeFile feedFile (show feeds)
    writeFile feedListFile $ unlines $ map Rss.url feeds
