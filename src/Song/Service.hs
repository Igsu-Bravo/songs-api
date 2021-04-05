module Song.Service where

import ClassyPrelude hiding (snoc)
import Control.Lens
import Data.Generics.Product
import Data.Has
import Song.Types

types Deps r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State = State
  { lastId :: Int,
    songs :: [Song]
  }
  deriving (Generic, Show)

{- lastId keeps track of generated ids to avoid duplicates -}
initialState :: State
initialState = State {lastId = 0, songs = []}

withTVar :: Deps r m => (TVar State -> STM a) -> m a
withTVar f = do
  tvar <- asks getter
  atomically $ f tvar

addSong :: Deps r m => CreateSong -> m Song
addSong createSong = withTVar  $ \tvar -> do
  state <- readTVar tvar
  let newId = 1 + state ^. field @"lastId"
      newSong = 
        Song { id = newId
            , title = createSong ^. field @"title"
            , completed = False
            }
      newState =
        State { lastId = newId
        , songs = snoc (state ^. field @"songs") newSong
        }
  writeTVar tvar newState
  return newSong

removeCompletedSongs :: Deps r m => m ()
removeCompletedSongs = withTVar $ \tvar ->
  modifyTVar' tvar $ \state ->
    state & field @"songs" %~ filter (not . getField @"completed")


getAllSongs :: Deps r m => m [Song]
getSong :: Deps r m => Int -> m (Maybe Song)
updateSong :: Deps r m => Int -> m ()
