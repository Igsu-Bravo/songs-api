module Song.Service where

import ClassyPrelude hiding (snoc)
import Control.Lens
import Data.Generics.Product
import Data.Has
import Song.Types

type Deps r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

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
getAllSongs = withTVar $ \tvar -> do
  state <- readTVar tvar
  return $ state ^. field @"songs"

getSong :: Deps r m => Int -> m (Maybe Song)
getSong songId = do
  songs <- getAllSongs
  return $ find (\song -> song ^. field @"id" == songId) songs

updateSong :: Deps r m => Int -> m ()
updateSong newSong = withTVar $ \tvar -> do
  state <- readTVar tvar
  let existingSongs = state ^. field @"songs"
      songId = newSong ^. field @"id"
      maySong = find (\song -> song ^. field @"id" == songId) existingSongs
  case maySong of
    Nothing ->
      return Nothing
    Just _ -> do
      let replace song =
            if song ^. field @"id" == songId
            then newSong
            else song
          newState = state & field @"songs" . traverse %~ replace
      writeTVar tvar newState
      return $ Just newSong

removeSong :: Deps r m => Int -> m ()
removeSong songId = withTVar $ \tvar ->
  modifyTVar' tvar $ \state ->
    state & field @"songs" %~ filter (\song -> song ^. field @"id" /= songId)
