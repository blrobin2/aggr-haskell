{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Album ( Album(..)
             ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Dates (Day)
import GHC.Generics (Generic)

data Album
  = Album
  { artist :: Text
  , title  :: Text
  , date   :: Day
  , score  :: Maybe Double
  } deriving (Generic, Eq, Show)

instance ToJSON Album where
  toJSON Album{..} = object
    [ "artist" .= artist
    , "title"  .= title
    , "date"   .= date
    ]

instance Ord Album where
  compare (Album a1 _ d1 _) (Album a2 _ d2 _) = compare d2 d1 <> compare a1 a2