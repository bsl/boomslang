module Main (main) where

import Game.Activity    (Activity(Starting))
import Game.Environment (environment)
import Game.G           (runG)
import Game.Logic       (logic)
import Game.Score       (Score(NoScore))
import Game.State       (State(State))
import qualified External.Graphics as ExternalGraphics

main :: IO ()
main = do
    ExternalGraphics.start
    runG logic environment (State Starting NoScore)
    ExternalGraphics.end
