module Main 
where

import qualified Rss
import UIUtils

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI.HSCurses.Widgets

import Control.Monad.State
import Control.Exception
import System

-- The RssData of our application consists of their feeds (including their 
-- items) and the selection view of the items
data RssData = RssData { 
     rssFeeds      :: [ Rss.RssFeed ] 
    ,rssFeedsView  :: SelectionView
    ,rssItemsView  :: SelectionView
}

-- We are encapsulating our state in our application within a state-monad
type RssState = State RssData

-- Creates a view for displaying all RSS-Feeds
newFeedView :: [Rss.RssFeed] -> SelectionView
newFeedView = ((flip newSelectionView) 0) . map Rss.name

-- Indicates the position of the widget
feedViewPosition :: Pos
feedViewPosition = (0, 0)

-- Indicates the size of the widget
feedViewSize :: Size
feedViewSize = (20, 10) -- (width, height)

-- Creates a view for an Rss-Item and displays all it's items
newItemView :: Rss.RssFeed -> SelectionView
newItemView = ((flip newSelectionView) 0) . map Rss.title . Rss.items

-- The position of the Rss-item
itemViewPosition :: Pos
itemViewPosition = (0, (fst feedViewSize) + 5)  -- (line, column)

-- The Size of the Rss-item
itemViewSize :: Size
itemViewSize = (45, 10)     -- (width, height)

moveFeedsDown :: SelectionView -> [Rss.RssFeed] -> ([Rss.RssFeed], SelectionView, SelectionView)
moveFeedsDown view feeds = (feeds, view', newItemView selectedFeed)
    where   selectedItem = selected view'
            selectedFeed = feeds !! selectedItem
            view'        = selectionViewMoveDown view

moveFeedsUp :: SelectionView -> [Rss.RssFeed] -> ([Rss.RssFeed], SelectionView, SelectionView)
moveFeedsUp view feeds = (feeds, view', newItemView selectedFeed)
    where   selectedItem = selected view'
            selectedFeed = feeds !! selectedItem
            view'        = selectionViewMoveUp view

mainloop :: [Rss.RssFeed] -> SelectionView -> SelectionView -> IO ()
mainloop rssFeeds feeds items = do
    wclear stdScr
    drawSelectionView feedViewPosition feedViewSize DHNormal feeds
    drawSelectionView itemViewPosition itemViewSize DHNormal items
    refresh
    key <- getKey $ return ()
    (rssFeeds', feeds', items') <- case key of 
        KeyChar 'q' -> exitWith ExitSuccess
        KeyChar 'J' -> return $ moveFeedsDown feeds rssFeeds
        KeyChar 'K' -> return $ moveFeedsUp feeds rssFeeds
        KeyChar 'j' -> return (rssFeeds, feeds, selectionViewMoveDown items)
        KeyChar 'k' -> return (rssFeeds, feeds, selectionViewMoveUp items)
        otherwise   -> return (rssFeeds, feeds, items)
    mainloop rssFeeds' feeds' items'

heiseItems :: [Rss.RssItem]
heiseItems = [
    Rss.newRssItem "Microsoft lässt sich Kampf mit Google Milliarden Dollar kosten" "content" "http://www.heise.de/newsticker/Microsoft-laesst-sich-Kampf-mit-Google-Milliarden-Dollar-kosten--/meldung/113337/from/rss09",
    Rss.newRssItem "Feinstaub: EU-Bürger dürfen \"Recht auf saubere Luft\" einklagen" "desc2" "http://www.heise.de/newsticker/Feinstaub-EU-Buerger-duerfen-Recht-auf-saubere-Luft-einklagen--/meldung/113331/from/rss09" ]

railscastsItems = map newItem [1..30]
    where   newItem num = Rss.newRssItem ("Railscast Episode " ++ strNum) 
                            ("content Episode " ++ strNum) ("http://episode")
                where   strNum = show num

testFeeds :: [Rss.RssFeed]
testFeeds = [ 
    Rss.newRssFeed "heise online News" 
        "http://www.heise.de/newsticker/heise.rdf" heiseItems, 
    Rss.newRssFeed "Railscasts"
        "http://feeds.feedburner.com/railscasts" railscastsItems ]

main :: IO ()
main = do
    start
    cursSet CursorInvisible
    mainloop testFeeds (newFeedView testFeeds) (newItemView (testFeeds !! 0))
    `finally` end

