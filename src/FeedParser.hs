module FeedParser (
    parseFeed
)
where

import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List

import qualified Rss

-- Parses an XML-Feed with the given content string
parseFeed :: String -> Rss.RssFeed
parseFeed content = Rss.newRssFeed (parseTitle doc) (parseUrl doc) (parseItems doc)
    where   parseResult = xmlParse "feed" content
            doc         = getContent parseResult
            getContent :: Document -> Content
            getContent (Document _ _ e _) = CElem e

parseChannel :: CFilter
parseChannel = tag "rss" /> tag "channel"

parseTitle :: Content -> String
parseTitle doc = contentToStringDefault "Undefined Title" 
    (parseChannel /> tag "title" /> txt $ doc)

parseUrl :: Content -> String
parseUrl doc = contentToStringDefault "Undefined Url"
    (parseChannel /> tag "link" /> txt $ doc)

parseItemsFilter :: CFilter
parseItemsFilter = parseChannel /> tag "item"

parseItems :: Content -> [Rss.RssItem]
parseItems doc = map parseItem (parseItemsFilter doc)
    where   parseItem :: Content -> Rss.RssItem
            parseItem item = Rss.newRssItem (parseItemTitle item) (parseItemContent item) (parseItemUrl item) False

parseItemTitle :: Content -> String
parseItemTitle doc = contentToStringDefault "Undefined Title" 
                        (keep /> tag "title" /> txt $ doc)

parseItemUrl :: Content -> String
parseItemUrl doc = contentToStringDefault "Undefined Url" 
                        (keep /> tag "link" /> txt $ doc)

parseItemContent :: Content -> String
parseItemContent doc = contentToStringDefault "Undefined Content"
                        (keep /> tag "description" /> txt $ doc)

contentToStringDefault :: String -> [Content] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ xs   = contentToString xs

contentToString :: [Content] -> String
contentToString = 
    concatMap procContent
    where procContent x = 
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x))

          fakeElem :: Content -> Element
          fakeElem x = Elem "fake" [] [x]

          unesc :: Element -> Element
          unesc = xmlUnEscape stdXmlEscaper
