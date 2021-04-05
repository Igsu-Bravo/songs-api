module Song.Service where

import ClassyPrelude hiding (snoc)
import Song.Types
import Control.Lens
import Data.Has
import Data.Generics.Product

data State = State
  { lastId :: Int
  , songs :: [Song]
  } deriving (Generic, Show)

{- lastId keeps track of generated ids to avoid duplicates -}
initialState :: State
initialState = State { lastId = 0, songs = [] }

types Deps r m = (Has (TVar State) r, MonadReader r m, MonadI0 m)

addSong :: Deps r m => CreateSong -> m Song

removeCompletedSongs :: Deps r m => m ()

getAllSongs :: Deps r m => m [Song]

getSong :: Deps r m => Int -> m (Maybe Song)

updateSong :: Deps r m => Int -> m ()

