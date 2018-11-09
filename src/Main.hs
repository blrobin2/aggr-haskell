{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, parseTimeM)
import           Data.Time.Format
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.XML
import           Text.XML.Cursor

data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Text
  } deriving (Generic, Eq, Show)

instance ToJSON Album where
  toEncoding = genericToEncoding defaultOptions

toAlbum :: [Text] -> (Text -> Album)
toAlbum [a, t]       = Album a t
toAlbum [a]          = Album a ""
toAlbum (a:t1:t2:[]) = Album a (t1 <> ": " <> t2)
toAlbum _            = error "What?"

withDate :: (Text -> Album) -> Text -> Album
withDate a d = a d

toUCTTime :: String -> Maybe UTCTime
toUCTTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z"

toDate :: Text -> Text
toDate d = case toUCTTime (T.unpack d) of
  Nothing -> ""
  Just d'  -> T.pack $ formatTime defaultTimeLocale "%b %d %Y" d'

main :: IO ()
main = do
  response <- httpLBS "https://pitchfork.com/rss/reviews/albums/"
  let document = parseLBS_ def (getResponseBody response)
  let cursor   = fromDocument document
  let albumArtist = cursor $// element "item" &/ element "title" &// content
  let date        = map toDate $ cursor $// element "item" &/ element "pubDate" &// content
  let albumsAwaitingDate = map (toAlbum . map (T.strip) . T.splitOn ":") albumArtist

  let albums = zipWith withDate albumsAwaitingDate date
  encodeFile "albums.json" albums