{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson (ToJSON, defaultOptions, encodeFile, genericToEncoding, toEncoding)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, parseTimeM)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           GHC.Generics (Generic)
import           Network.HTTP.Simple (Request, httpLBS, getResponseBody)
import           Text.XML (def, parseLBS_)
import           Text.XML.Cursor (Cursor, content, element, fromDocument, ($/), ($//), (&/), (&//))

data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Text
  } deriving (Generic, Eq, Show)

instance ToJSON Album where
  toEncoding = genericToEncoding defaultOptions

getPitchforkAlbums :: IO [Album]
getPitchforkAlbums = do
  cursor <- getXmlCursor "https://pitchfork.com/rss/reviews/albums/"
  let albumsAwaitingDate = toAlbumAwaitingDate $ geArtistAndTitle cursor
  let date = getReleaseDate cursor
  return $ zipWith withDate albumsAwaitingDate date

getXmlCursor :: Request -> IO Cursor
getXmlCursor url = do
  response <- httpLBS url
  let xmlDocument = parseLBS_ def (getResponseBody response)
  return $ fromDocument xmlDocument

geArtistAndTitle :: Cursor -> [Text]
geArtistAndTitle cursor =
  cursor $// element "item" &/ element "title" &// content

getReleaseDate :: Cursor -> [Text]
getReleaseDate cursor = map toDate dates
  where dates = cursor $// element "item" &/ element "pubDate" &// content

toAlbumAwaitingDate :: [Text] -> [(Text -> Album)]
toAlbumAwaitingDate = map (toAlbum . map T.strip . T.splitOn ":")

toAlbum :: [Text] -> (Text -> Album)
toAlbum [a, t]       = Album a t
toAlbum [a]          = Album a ""
toAlbum (a:t1:t2:[]) = Album a (t1 <> ": " <> t2)
toAlbum _            = error "Invalid pattern!"

withDate :: (Text -> Album) -> Text -> Album
withDate a d = a d

toUCTTime :: String -> Maybe UTCTime
toUCTTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

toDate :: Text -> Text
toDate d = case toUCTTime (T.unpack d) of
  Nothing  -> ""
  Just d'  -> T.pack $ formatTime defaultTimeLocale "%b %d %Y" d'

main :: IO ()
main = do
  albums <- getPitchforkAlbums
  encodeFile "albums.json" albums