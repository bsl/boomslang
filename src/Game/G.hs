{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.G
  ( -- *  Types
    G
    -- *  Utilities
  , runG
    -- *  Reexported
    -- ** Environment
  , ask
  , asks
    -- ** Randomness
  , getRandomR
    -- ** State
  , get
  , put
    -- ** IO
  , liftIO
  )
  where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad.Random       (MonadRandom, RandT, StdGen, evalRandT, getRandomR, newStdGen)
import Control.Monad.Reader       (MonadReader, ReaderT, runReaderT, ask, asks)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans        (MonadIO, liftIO)

import Game.Environment (Environment)
import Game.State       (State)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype G a = G (ReaderT Environment (RandT StdGen (StateT State IO)) a)
  deriving (Functor, Monad, MonadIO, MonadRandom, MonadReader Environment, MonadState State)

runG :: G a -> Environment -> State -> IO a
runG (G r) e s = do
    sg <- newStdGen
    evalStateT (evalRandT (runReaderT r e) sg) s
