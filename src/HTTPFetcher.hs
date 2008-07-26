module HTTPFetcher (
    fetchFeed
)
where

import Network.HTTP
import Network.URI
import Data.Maybe
import qualified Control.Exception as Ex

-- Fetches the content from the given URL
fetchFeed :: String -> IO String
fetchFeed url = do
    response <- simpleHTTP request
    case response of
        Left x  -> return ""
        Right r -> case rspCode r of
            (2, _, _) -> return $ rspBody r
            (3, _, _) -> case findHeader HdrLocation r of
                Nothing  -> return $ rspBody r
                Just url -> fetchFeed url
            _ -> return ""
    `Ex.catch` (\_ -> return "")

    where   request = Request {
                        rqURI     = uri,
                        rqMethod  = GET,
                        rqHeaders = [],
                        rqBody    = ""
                      }
            uri = fromJust $ parseURI url
