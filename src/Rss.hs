module Rss (
     RssFeed
    ,RssItem
    ,newRssFeed
    ,newRssItem
    ,title
    ,content
    ,homepage
    ,name
    ,url
    ,items
    ,addRssItem
)
where

type Title   = String
type Content = String
type URL     = String

-- This type represents a Rss-Item used for displaying RSS-Items
data RssItem = RssItem {
    title    :: Title,
    content  :: Content,
    homepage :: URL
} deriving (Show, Eq, Ord)

-- This type represents a RSS-Feed which consists of many RSS-Items
data RssFeed = RssFeed { 
    name    :: Title,
    url     :: URL,
    items   :: [RssItem]
} deriving (Show, Eq, Ord)

-- Constructs a new RssFeed
newRssFeed :: Title -> URL -> [RssItem] -> RssFeed
newRssFeed = RssFeed

-- Constructs a new RssItem
newRssItem :: Title -> Content -> URL -> RssItem
newRssItem = RssItem

-- Adds a new item to the feed
addRssItem :: RssFeed -> RssItem -> RssFeed
addRssItem feed item = RssFeed (name feed) (url feed) ((items feed) ++ [item])

