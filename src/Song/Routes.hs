module Song.Routes where

import ClassyPrelude hiding (delete)
import Control.Lens
import Data.Generics.Product hiding (param)
import Network.HTTP.Types.Status
import Network.Wai
import qualified Song.Service as S
import Web.Scotty.Trans
import Song.Types

routes :: S.Deps r m => ScottyT LText m ()
routes = do
  post "/songs" $ do
    arg <- jsonData
    result <- lift $ S.addSong arg
    json result

  get "/songs" $ do
    result <- lift S.getAllSongs
    json result

  delete "/songs" $ do
    lift S.removeCompletedSongs
    status status204

  get "/songs/:id" $ do
    songId <- param "id"
    mayResult <- lift $ S.getSong songId
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result

  put "/songs/:id" $ do
    songId <- param "id"
    arg :: UpdateSong <- jsonData
    let song = Song {
      id = songId,
      title = arg ^. field @"title",
      completed = arg ^. field @"completed"
    }
    mayResult <- lift $ S.updateSong song
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result

  delete "/songs/:id" $ do
    songId <- param "id"
    lift $  S.removeSong songId
    status status204