module Lib (main) where

import ClassyPrelude
import Song.Routes as SongR
import Song.Service as SongS
import Web.Scotty.Trans

type Env = TVar SongS.State

type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT

main :: IO ()
main = do
  env <- newTVarIO SongS.initialState
  scottyT 3000 (runApp env) SongR.routes
