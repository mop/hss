module ConfigLoader (
     loadFeeds
    ,fetchFeeds
    ,writeFeeds
    ,mergeFeeds
    ,appendFeed
)
where

import qualified Rss
import FeedParser
import HTTPFetcher
import Control.Monad
import qualified Control.Exception as Ex
import IO

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
                                    else x { 
                                        Rss.items = mergeItems 
                                            (Rss.items oldItem) 
                                            (Rss.items x) 
                                    } : ys
                where   oldItems    = filter sameTitle readFeeds
                        sameTitle y = Rss.name x == Rss.name y
                        oldItem     = head oldItems

--- Merges two Rss-Item-lists
mergeItems :: [Rss.RssItem] -> [Rss.RssItem] -> [Rss.RssItem]
mergeItems readItems newItems = foldr replaceItems [] newItems
    where   replaceItems x ys = if length oldItems == 0 
                                    then x : ys
                                    else (head oldItems) : ys
                where   oldItems    = filter sameTitle readItems
                        sameTitle y = Rss.title x == Rss.title y


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

-- Appends a new feed to the fetch-feed-list
appendFeed :: String -> IO ()
appendFeed url = do
    appendFile feedListFile (url ++ "\n")
