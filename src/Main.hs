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
newItemView feed = newItemViewWithPos feed 0

-- Creates a view for an Rss-Item and displays all it's items with a given pos
newItemViewWithPos :: Rss.RssFeed -> Int -> SelectionView
newItemViewWithPos feed pos = newSelectionView ((map printItem . Rss.items) 
        feed) pos
    where   printItem item = if Rss.isRead item
                                then Rss.title item
                                else "+ " ++ (Rss.title item)


-- The position of the Rss-item
itemViewPosition :: Pos
itemViewPosition = (0, (fst feedViewSize) + 5)  -- (line, column)

-- The Size of the Rss-item
itemViewSize :: Size
itemViewSize = (45, 10)     -- (width, height)

-- Returns the position of the content view
contentViewPosition :: Pos
contentViewPosition = (11, 1) -- (line, col)

-- Returns the size of the content view
contentViewSize :: Size
contentViewSize = (10, 77)  -- (height, width)

-- Creates a new contentView from a given RSS-Item
contentView :: Rss.RssItem -> TextWidget
contentView item = newTextWidget defaultTWOptions joinedContent
    where   splittedContent = (Rss.content item) `inGroupsOf` 
                                 (snd contentViewSize)
            joinedContent   = unlines splittedContent 

moveFeedsDown :: SelectionView -> [Rss.RssFeed] -> 
    ([Rss.RssFeed], SelectionView, SelectionView)
moveFeedsDown = moveFeeds selectionViewMoveDown

moveFeedsUp :: SelectionView -> [Rss.RssFeed] -> 
    ([Rss.RssFeed], SelectionView, SelectionView)
moveFeedsUp = moveFeeds selectionViewMoveUp

-- This function moves the feeds in the list. It takes an move-function
-- (selectionViewMoveUp or selectionViewMoveDown) which will be applied to the
-- given SelectionView. Moreover a feeds-list must be given in order to return
-- a new Item-View for the Rss-Feed-Items.
moveFeeds :: (SelectionView -> SelectionView) -> SelectionView -> 
    [Rss.RssFeed] -> ([Rss.RssFeed], SelectionView, SelectionView)
moveFeeds fun view feeds = (feeds, view', newItemView selectedFeed)
    where   selectedItem = selected view'
            selectedFeed = feeds !! selectedItem
            view'        = fun view

-- Marks the selected feeds as read
-- rssFeeds: A list of RSS-Feeds which should be marked
-- feeds: The view for the rssFeeds in order to determine which feed is 
--        selected
-- items: The view for the items to mark them as read
markSelected :: [Rss.RssFeed] -> SelectionView -> SelectionView -> 
    ([Rss.RssFeed], SelectionView)
markSelected rssFeeds feeds items = (newFeeds, newView)
    where   feedPos      = (selected feeds)
            itemPos      = (selected items)
            newFeeds     = Rss.markRssItemInFeed rssFeeds feedPos itemPos True
            newView      = newItemViewWithPos (newFeeds !! feedPos) itemPos

selectedItem :: [Rss.RssFeed] -> SelectionView -> SelectionView -> Rss.RssItem
selectedItem rssFeeds feeds items = (Rss.items feed) !! selectedRssItem
    where   selectedFeed    = selected feeds
            selectedRssItem = selected items
            feed            = (rssFeeds !! selectedFeed)

mainloop :: [Rss.RssFeed] -> SelectionView -> SelectionView -> IO ()
mainloop rssFeeds feeds items = do
    wclear stdScr
    (rssFeeds', items') <- return (markSelected rssFeeds feeds items)
    drawSelectionView feedViewPosition feedViewSize DHNormal feeds
    drawSelectionView itemViewPosition itemViewSize DHNormal items'
    drawTextWidget contentViewPosition contentViewSize DHNormal (contentView (selectedItem rssFeeds' feeds items))
    refresh
    key <- getKey $ return ()
    (rssFeeds'', feeds', items'') <- case key of 
        KeyChar 'q' -> exitWith ExitSuccess
        KeyChar 'J' -> return $ moveFeedsDown feeds rssFeeds'
        KeyChar 'K' -> return $ moveFeedsUp feeds rssFeeds'
        KeyChar 'j' -> return (rssFeeds', feeds, selectionViewMoveDown items')
        KeyChar 'k' -> return (rssFeeds', feeds, selectionViewMoveUp items')
        otherwise   -> return (rssFeeds', feeds, items')
    mainloop rssFeeds'' feeds' items''

heiseItems :: [Rss.RssItem]
heiseItems = [
    Rss.newRssItem "Microsoft lässt sich Kampf mit Google Milliarden Dollar kosten" "content" "http://www.heise.de/newsticker/Microsoft-laesst-sich-Kampf-mit-Google-Milliarden-Dollar-kosten--/meldung/113337/from/rss09" False,
    Rss.newRssItem "Feinstaub: EU-Bürger dürfen \"Recht auf saubere Luft\" einklagen" ((replicate 100 '-') ++ "\n" ++ (replicate 100 '-')) "http://www.heise.de/newsticker/Feinstaub-EU-Buerger-duerfen-Recht-auf-saubere-Luft-einklagen--/meldung/113331/from/rss09" False]

railscastsItems = map newItem [1..30]
    where   newItem num = Rss.newRssItem ("Railscast Episode " ++ strNum) 
                            ("content Episode " ++ strNum) ("http://episode") False
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

