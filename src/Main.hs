module Main 
where

import UIUtils
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI.HSCurses.Widgets

import Control.Monad.State
import Control.Exception

import qualified ConfigLoader as Ql
import System
import StringPrettifier

import qualified Rss

-- This stores the global state of the whole application
data StateData = StateData {
    rssFeeds      :: [Rss.RssFeed],     -- Every feed with items
    feedsView     :: SelectionView,     -- The view for the feed-list
    itemsView     :: SelectionView,     -- The view for the items of the 
                                        -- selected feed
    contentView   :: TextWidget,

    feedsPosition :: Pos,               -- The position of the feedsView
    feedsSize     :: Size,              -- The size of the feedsView

    itemsPosition :: Pos,               -- The position of the itemsView
    itemsSize     :: Size,              -- The size of the itemsView

    contentPosition :: Pos,             -- The position of the content view
    contentSize     :: Size             -- The size of the content view
}

-- Our StateT type synonym
type RssState = StateT StateData IO

-- Refreshes the feedsView
refreshFeeds :: RssState ()
refreshFeeds = do
    state <- get
    lift $ drawSelectionView (feedsPosition state) (feedsSize state) DHNormal 
        (feedsView state)

-- Refreshes all items
refreshItems :: RssState ()
refreshItems = do
    state <- get

    let itemsView' = newItemsView (rssFeeds state) (selected $ feedsView state)
    let itemsView'' = itemsView' { selected = selected (itemsView state) }
    lift $ drawSelectionView (itemsPosition state) (itemsSize state) DHNormal
        itemsView''
    put state {
        itemsView = itemsView''
    }

selectedItem :: StateData -> Maybe Rss.RssItem
selectedItem state = case feeds of
                        [] -> Nothing
                        xs -> Just selectedItem
    where   selectedFeed = selected $ feedsView state
            feeds = rssFeeds state
            selectedItems = Rss.items (feeds !! selectedFeed)
            selectedItem  = selectedItems !! (selected $ itemsView state)

-- refreshes the content
refreshContent :: RssState ()
refreshContent = do
    state <- get
    let view = contentView state
    lift $ drawTextWidget (contentPosition state) 
                (contentSize state) DHNormal view
    return ()

-- Refreshes all views
refreshHss :: RssState ()
refreshHss = do
    lift (wclear stdScr)
    refreshFeeds
    refreshItems
    refreshContent
    lift refresh

-- Fetches the feeds from the internet
fetchFeeds :: RssState ()
fetchFeeds = do
    state <- get
    fetchedFeeds <- lift Ql.fetchFeeds
    let mergedFeeds = Ql.mergeFeeds (rssFeeds state) fetchedFeeds
    let feedsView'  = newFeedsView mergedFeeds $ selected $ feedsView state
    let itemsView'  = newItemsView mergedFeeds $ selected $ feedsView state
    put state {
        rssFeeds  = mergedFeeds,
        feedsView = feedsView',
        itemsView = itemsView'
    }

-- creates a new itemView
-- [Rss.RssFeed] are the list of RSS-Feeds
-- Int is the selected item
newItemsView :: [Rss.RssFeed] -> Int -> SelectionView
newItemsView [] _ = newSelectionView [] 0
newItemsView xs pos = newSelectionView (map itemsDesc (selectedItems)) 0
    where   selectedItems  = Rss.items (xs !! pos)
            itemsDesc item = if Rss.isRead item 
                                then Rss.title item
                                else "+" ++ Rss.title item

-- Creates a new feed view
-- the first parameter is the list of rss-feeds
-- the second parameter is the position which should be marked
newFeedsView :: [Rss.RssFeed] -> Int -> SelectionView
newFeedsView [] _ = newSelectionView [] 0
newFeedsView xs pos = newSelectionView (map feedsDesc xs) safePos
    where   feedsDesc = Rss.name
            safePos | pos >= length xs = length xs - 1
                    | otherwise        = pos

newContentView :: Maybe Rss.RssItem -> Int -> TextWidget
newContentView Nothing _ = 
    newTextWidget defaultTWOptions ""
newContentView (Just item) width = 
    newTextWidget defaultTWOptions $ prettify width (Rss.content item)

-- Moves the list of feeds up or down depending on the given function. The
-- function might be selectionViewMoveDown or selectionViewMoveUp
moveFeeds :: (SelectionView -> SelectionView) -> RssState ()
moveFeeds fun = do
    state <- get
    let feedsView'   = fun $ feedsView state 
    let itemsView'   = newItemsView (rssFeeds state) (selected feedsView')
    let contentView' = newContentView (selectedItem state) (snd (contentSize state))
    put state {
        feedsView   = feedsView',
        itemsView   = itemsView',
        contentView = contentView'
    }

-- Moves the selection of the feeds-list down
moveFeedsDown :: RssState ()
moveFeedsDown = moveFeeds selectionViewMoveDown

-- Moves the selection of the feeds-list up
moveFeedsUp :: RssState ()
moveFeedsUp = moveFeeds selectionViewMoveUp

-- Moves the items-view depending on the given function. see moveFeeds
moveItems :: (SelectionView -> SelectionView) -> RssState ()
moveItems fun = do
    state <- get
    let itemsView' = fun (itemsView state)
    let content = newContentView (selectedItem state) (snd (contentSize state))
    put state {
        itemsView = itemsView',
        contentView = content
    }

-- moves the selection of the items-list down
moveItemsDown :: RssState ()
moveItemsDown = moveItems selectionViewMoveDown

-- moves the selection of the items-list up
moveItemsUp :: RssState ()
moveItemsUp = moveItems selectionViewMoveUp

scrollContentDown :: RssState ()
scrollContentDown = scrollContent textWidgetScrollDown

scrollContentUp :: RssState ()
scrollContentUp = scrollContent textWidgetScrollUp

scrollContent :: (Size -> TextWidget -> TextWidget) -> RssState ()
scrollContent fun = do
    state <- get
    let size     = contentSize state
    let content  = contentView state
    let content' = fun size content
    put state {
        contentView = content'
    }

editWidget :: EditWidget
editWidget = newEditWidget defaultEWOptions ""

--- Creates a new feed
--- It calls fetchFeeds to refetch the feeds
createFeed :: RssState ()
createFeed = do
    state <- get
    let line = (fst $ contentPosition state) + (fst $ contentSize state) + 2
    (_, s) <- lift (activateEditWidget resizer (line, 1) (1, 77) editWidget)
    if s /= ""
        then do
            lift $ Ql.appendFeed s
            fetchFeeds
        else return ()

--- Deletes the selected feed and updates the state-monad
--- It calls fetchFeeds to refetch the feeds
deleteFeed :: RssState ()
deleteFeed = do
    state <- get
    if length (rssFeeds state) > 0
        then do
            let pos = (selected $ feedsView state)
            put state { 
                rssFeeds = removeElementAt pos $ rssFeeds state
            }
            state' <- get
            lift $ Ql.writeFeeds $ rssFeeds state'
            fetchFeeds
        else return()

--- Removes the element at the given position.
removeElementAt :: Int -> [a] -> [a]
removeElementAt num list = fst split ++ (drop 1 $ snd split)
    where   split = splitAt num list

-- TODO: make this work... :(
resizer :: IO ()
resizer = do
    return ()
--    endWin
--    update
--    refresh
--    size <- scrSize
--    wAddStr stdScr $ show size

-- handles the input
handleInput :: RssState ()
handleInput = do
    state <- get
    key <- lift (getKey $ resizer)
    case key of 
        KeyChar 'a' -> createFeed
        KeyChar 'x' -> deleteFeed
        KeyChar 'r' -> fetchFeeds
        KeyChar 'J' -> moveFeedsDown
        KeyChar 'K' -> moveFeedsUp
        KeyChar 'j' -> moveItemsDown
        KeyChar 'k' -> moveItemsUp
        KeyChar 'l' -> scrollContentDown
        KeyChar 'h' -> scrollContentUp
        KeyChar 'q' -> do
                lift (Ql.writeFeeds $ rssFeeds state) 
                lift $ exitWith ExitSuccess
        otherwise -> return ()
    return ()

markSelected :: RssState ()
markSelected = do
    state <- get
    let feedsPos = selected $ feedsView state
    let itemsPos = selected $ itemsView state
    let feeds = Rss.markRssItemInFeed (rssFeeds state) feedsPos itemsPos True
    put state {
        rssFeeds    = feeds
    }

-- here gets the actual work done
mainloop :: RssState ()
mainloop = do 
    markSelected
    refreshHss
    handleInput
    mainloop

-- Helper for running the state
runRssState :: StateData -> IO ((), StateData)
runRssState = runStateT mainloop 

-- the main function which initializes the apps and runs the state
main :: IO ()
main = do
    start
    cursSet CursorInvisible
    feeds <- Ql.loadFeeds       -- load the feeds from the HD
    let selected = if length feeds > 0
                    then if length (Rss.items (feeds !! 0)) > 0
                        then Just $ (Rss.items (feeds !! 0)) !! 0
                        else Nothing
                    else Nothing
    (_, state) <- runRssState StateData {
        rssFeeds = feeds,
        feedsView = newFeedsView feeds 0,
        itemsView = newItemsView feeds 0,
        contentView = newContentView selected 77 ,
        feedsPosition = (0, 0),
        feedsSize = (20, 10),
        itemsPosition = (0, 25),
        itemsSize = (45, 10),
        contentPosition = (11, 1),
        contentSize = (10, 77)
    }
    putStrLn "bye"
    `finally` end

