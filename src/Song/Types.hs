module Songs.Types where

import ClassyPrelude
import Data.Aeson
import Data.Generics.Product
import GHC.Generics (Generic)

newtype CreateSong = CreateSong
  { title :: Text
  }
  deriving (Generic, Show)

instance ToJSON CreateSong

instance FromJSON CreateSong

data UpdateSong = UpdateSong
  { title :: Text,
    completed :: Bool
  }
  deriving (Generic, Show)

instance ToJSON UpdateSong

instance FromJSON UpdateSong

data Song = Song
  { id :: Int
  , title :: Text
  , completed :: Bool
  } deriving (Generic, Show)

instance ToJSON Song
instance FromJSON Song