module Rss (
     RssFeed
    ,RssItem
    ,newRssFeed
    ,newRssItem
    ,title
    ,content
    ,homepage
    ,name
    ,isRead
    ,setRead
    ,url
    ,items
    ,addRssItem
    ,markRssItem
    ,markRssItemInFeed
)
where

import Data.List(foldl')

type Title   = String
type Content = String
type URL     = String

-- This type represents a Rss-Item used for displaying RSS-Items
data RssItem = RssItem {
    title    :: Title,
    content  :: Content,
    homepage :: URL,
    isRead   :: Bool
} deriving (Show, Read, Eq, Ord)

-- Sets the given item as read
setRead :: RssItem -> Bool -> RssItem
setRead item read = newRssItem (title item) (content item) 
    (homepage item) read

-- This type represents a RSS-Feed which consists of many RSS-Items
data RssFeed = RssFeed { 
    name    :: Title,
    url     :: URL,
    items   :: [RssItem]
} deriving (Show, Read, Eq, Ord)

-- Constructs a new RssFeed
newRssFeed :: Title -> URL -> [RssItem] -> RssFeed
newRssFeed = RssFeed

-- Constructs a new RssItem
newRssItem :: Title -> Content -> URL -> Bool -> RssItem
newRssItem = RssItem

-- Adds a new item to the feed
addRssItem :: RssFeed -> RssItem -> RssFeed
addRssItem feed item = RssFeed (name feed) (url feed) ((items feed) ++ [item])

-- sets Rss items
setRssItems :: RssFeed -> [RssItem] -> RssFeed
setRssItems feed items = newRssFeed (name feed) (url feed) items

-- marks an item in the feed as read
-- An RssFeed must be given, an integer which indicates the position of the rss
-- item and the boolean value to which the isRead-flag should be set must be
-- given. A new RssFeed will be returned
markRssItem :: RssFeed -> Int -> Bool -> RssFeed
markRssItem feed position flag = newRssFeed (name feed) (url feed) 
        $ findAndReplace (\x -> setRead x flag) (items feed) position 

-- marks an item in the list of feeds as read
-- feeds: is a list of feeds
-- feedPos: Indicates the feed within the given position
-- itemPos: Indicates the item within the given position
-- readFlag: Indicates the flag which should be used to mark the
-- isRead-Attribute
-- Returns: A new RssFeed-list is returned
markRssItemInFeed :: [RssFeed] -> Int -> Int -> Bool -> [RssFeed]
markRssItemInFeed [] _ _ _ = []
markRssItemInFeed feeds feedPos itemPos readFlag = newFeedList
    where   markedFeed  = markRssItem selectedFeed itemPos readFlag
            newFeedList = findAndReplace (\x -> markedFeed) feeds feedPos
            selectedFeed = if feedPos >= (length feeds)
                               then feeds !! ((length feeds) - 1)
                               else feeds !! feedPos


-- finds the item on the given position and applies the given function on it. A
-- new (updated) list of items will be returned.
findAndReplace :: (a -> a) -> [a] -> Int -> [a]
findAndReplace fun xs pos  = reverse $ snd $ foldl' copyPosition (pos, []) xs
    where   copyPosition (position, ys) x 
                | position == 0 = (position - 1, (fun x) : ys)
                | otherwise     = (position - 1, x : ys)
