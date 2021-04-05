module Song.Routes where

import ClassyPrelude hiding (delete)
import Control.Lens
import Data.Generics.Product hiding (param)
import Network.HTTP.Types.Status
import Network.Wai
import qualified Song.Service as S
import Web.Scotty.Trans

routes :: S.Deps r m => ScottyT LText m ()
routes = do
  post "/songs" $ do
    arg <- jsonData
    result <- lift $ S.addSong arg
    json result

  get "/songs" undefined
  delete "/songs" undefined
  get "/songs/:id" undefined
  put "/songs/:id" undefined
  delete "/songs/:id" undefined