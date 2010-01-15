module Game.Logic (logic) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad (replicateM, liftM2, unless, when)
import Data.List     ((\\))

import Data.Accessor.Basic ((^.), (^=), (^:))

import External.Graphics.Rendering (renderScene)
import External.Time               (readTimer, sleep)
import Game.Activity               (Activity(..))
import Game.Entity.Dot             (Dot)
import Game.Entity.Dot.Activity    (Activity(..))
import Game.G                      (G, ask, get, getRandomR, put, modify)
import Game.Level                  (Level(..), numDots, numDotsRequired)
import Game.Score                  (Score(..))
import Vector                      ((^-^), (^+^), (.*^))
import qualified External.Input.Keyboard      as Keyboard
import qualified External.Input.Mouse         as Mouse
import qualified Game.Entity.Dot              as Dot
import qualified Game.Entity.Dot.Activity     as DotActivity
import qualified Game.Environment             as Environment
import qualified Game.Level                   as Level
import qualified Game.Score                   as Score
import qualified Game.State                   as State
import qualified Graphics.Rendering.OpenGL    as GL
import qualified Vector                       as Vector

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

logic :: G ()
logic =
    loopUntilUserQuits $ do
        t1 <- readTimer
        s <- get
        case s^.State.activity of
          Starting                               -> start
          PreparingLevel level                   -> prepareLevel level
          Playing level dots                     -> play level dots
          Colliding level activeDots roamingDots -> collide level activeDots roamingDots
          EndingLevel level dots                 -> endLevel level dots
          Quitting                               -> return ()
        t2 <- readTimer
        let diff = t2 - t1
        when (diff < limit) (sleep $ limit - diff)
  where
    limit = 1/60

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

unitStep :: Double -> Double
unitStep n =
    if n < 0
      then 0
      else 1

updateDotRadius :: Dot -> G Dot
updateDotRadius dot = do
    t <- readTimer
    let n = t - dot^.Dot.timestamp
    case dot^.Dot.activity of
      Roaming ->
        if n <= 0.1
          then let r = 10 * n * normal
               in return $ Dot.radius ^= r $ dot
          else return dot
      Hit ->
        if n > 3
           then updateActivity None dot
           else let r = if n < 2
                          then normal + adjust * (unitStep (n-exp(-growth))*(log n + growth))/growth
                          else splode * exp (decay * (n-2))
                in return $ Dot.radius ^= r $ dot
      None -> return $ Dot.radius ^= 0 $ dot
  where
    normal = 0.04
    splode = 0.16
    adjust = abs $ splode - normal
    growth = 5
    decay  = negate growth

updateActivity :: DotActivity.Activity -> Dot -> G Dot
updateActivity a d = do
    t <- readTimer
    let dot = Dot.activity  ^= a
            $ Dot.timestamp ^= t $ d
    return dot

activate :: [Dot] -> [Dot] -> G ([Dot],[Dot])
activate activeDots roamingDots = do
    let newActiveDots = filter (\rd -> any (areColliding rd) activeDots) roamingDots
        roamingDots' = roamingDots \\ newActiveDots
    newActiveDots' <- mapM (updateActivity Hit) newActiveDots
    return (activeDots ++ newActiveDots', roamingDots')
  where
    areColliding d0 d1 =
        xDistance < maxDistance &&
        yDistance < maxDistance &&
        vDistance < maxDistance
      where
        xDistance = abs $ x1 - x2
        yDistance = abs $ y1 - y2
        vDistance = Vector.vlen (d0p ^-^ d1p)
        maxDistance = realToFrac $ d0^.Dot.radius + d1^.Dot.radius
        d0p = d0^.Dot.position
        d1p = d1^.Dot.position
        x1 = Vector.getX d0p
        y1 = Vector.getY d0p
        x2 = Vector.getX d1p
        y2 = Vector.getY d1p

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

makeDots :: Integer -> G [Dot]
makeDots n =
    replicateM (fromIntegral n) makeDot

makeDot :: G Dot
makeDot = do
    e <- ask

    -- radius
    let radius = e^.Environment.normalDotRadius

    -- color
    rc <- realToFrac `fmap` randomFloatR 0 1
    gc <- realToFrac `fmap` randomFloatR 0 1
    bc <- realToFrac `fmap` randomFloatR 0 1
    let color = GL.Color4 rc gc bc 0.6

    -- position
    px <- randomDoubleR (radius-1) (1-radius)
    py <- randomDoubleR (radius-1) (1-radius)
    let position = Vector.V (realToFrac px) (realToFrac py)

    -- direction
    dx_ <- randomDoubleR (1/4) (3/4)
    let dy_ = sqrt $ 1-dx_**2
    dx <- fmap (\b -> if b then negate dx_ else dx_) randomBool
    dy <- fmap (\b -> if b then negate dy_ else dy_) randomBool
    let direction = Vector.V (realToFrac dx) (realToFrac dy)

    -- velocity
    let velocity = e^.Environment.dotVelocity

    -- activity
    let activity = Roaming

    t <- readTimer

    return $ Dot.make radius color position direction velocity activity t
  where
    randomFloatR l h  = getRandomR (l,h)              :: G Float
    randomDoubleR l h = getRandomR (l,h)              :: G Double
    randomBool        = fmap (== 0) (getRandomR (0,1) :: G Int)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

updateDots :: [Dot] -> G [Dot]
updateDots =
    fmap (filter isAlive) . mapM updateDot
  where
    isAlive d = d^.Dot.activity /= None

updateDot :: Dot -> G Dot
updateDot dot =
    case dot^.Dot.activity of
      Roaming -> updateRoamingDot dot
      Hit     -> return dot
      None    -> return dot
    >>= updateDotRadius

updateRoamingDot :: Dot -> G Dot
updateRoamingDot dot = do
    let d   = dot^.Dot.direction
        dx  = Vector.getX d
        dy  = Vector.getY d
        p   = dot^.Dot.position
        r   = dot^.Dot.radius
        v   = dot^.Dot.velocity
        p'  = p ^+^ (realToFrac v .*^ d)
        p'x = realToFrac $ Vector.getX p'
        p'y = realToFrac $ Vector.getY p'
        (p'x',fdx) =
          if p'x+r > 1
            then (2-p'x-(2*r),True)
            else
              if p'x-r < -1
                then (-2-p'x+(2*r),True)
                else (p'x,False)
        (p'y',fdy) =
          if p'y+r > 1
            then (2-p'y-(2*r),True)
            else
              if p'y-r < -1
                then (-2-p'y+(2*r),True)
                else (p'y,False)
        p'' = Vector.V (realToFrac p'x') (realToFrac p'y')
        d'' = Vector.V
               (if fdx then negate dx else dx)
               (if fdy then negate dy else dy)
    return $ Dot.position  ^= p'' $
             Dot.direction ^= d'' $ dot

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

loopUntilUserQuits :: G () ->  G ()
loopUntilUserQuits f = do
    done <- liftM2 (||) (Keyboard.pressed Keyboard.escape) (Keyboard.pressed Keyboard.q)
    unless done (f >> loopUntilUserQuits f)

start :: G ()
start = modify $ (State.activity ^= PreparingLevel Level1) . (State.score ^= NoScore)

prepareLevel :: Level -> G ()
prepareLevel level = do
    s <- get
    dots <- makeDots nd
    put $ State.score    ^= Score nd ndr 0 (np s) $
          State.activity ^= Playing level dots    $ s
  where
    nd   = numDots         level
    ndr  = numDotsRequired level
    np s = if s^.State.score == NoScore then 0 else s^.State.score^.Score.numPoints

play :: Level -> [Dot] -> G ()
play level dots = do
    e <- ask
    dots' <- updateDots dots
    mxy   <- Mouse.clicked Mouse.left
    case mxy of
      Nothing -> modify $ State.activity ^= Playing level dots'
      Just (x,y) -> do
        t <- readTimer
        let radius    = e^.Environment.normalDotRadius
            color     = e^.Environment.placedDotColor
            velocity  = e^.Environment.dotVelocity
            position  = Vector.V (realToFrac x) (realToFrac y)
            direction = Vector.vnull
            activity  = Hit
            dot       = Dot.make radius color position direction velocity activity t
        modify $ State.activity ^= Colliding level [dot] dots'
    renderScene

collide :: Level -> [Dot] -> [Dot] -> G ()
collide level activeDots roamingDots
  | null activeDots = do
      let activity' = EndingLevel level dots'
          dots'     = map (Dot.activity ^= Hit) roamingDots
      modify $ State.activity ^= activity'
  | otherwise = do
      activeDots'  <- updateDots activeDots
      roamingDots' <- updateDots roamingDots
      (activeDots'', roamingDots'') <- activate activeDots' roamingDots'

      s <- get
      let a'  = Colliding level activeDots'' roamingDots''
          p   = fromIntegral $ length activeDots'' - length activeDots'
          sc  = s^.State.score
          sc' = Score.numDotsActivated ^: (p +) $ sc

      put $ State.activity ^= a'  $
            State.score    ^= sc' $ s
      renderScene

endLevel :: Level -> [Dot] -> G ()
endLevel level dots = do
    dots' <- updateDots dots
    s <- get
    if null dots'
      then do
          let sc = s^.State.score
          if (sc^.Score.numDotsActivated) >= (sc^.Score.numDotsRequired)
            then
              case Level.next level of
                Just level' -> put $ State.activity ^= PreparingLevel level'                      $
                                     State.score^:Score.numPoints^:(+ sc^.Score.numDotsActivated) $ s
                Nothing     -> put $ State.activity ^= Starting                                   $
                                     State.score^:Score.numPoints^:(+ sc^.Score.numDotsActivated) $ s
            else put $ State.activity ^= PreparingLevel level $ s
      else do
          put $ State.activity ^= EndingLevel level dots' $ s
          renderScene
