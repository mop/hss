import FeedParser
import HTTPFetcher
main = do
    feed <- fetchFeed "http://cyber.law.harvard.edu/rss/examples/rss2sample.xml"
    print $ show $ parseFeed feed

