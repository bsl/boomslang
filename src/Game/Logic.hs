module Game.Logic (logic) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad      (replicateM, when)
import Data.Array.Unboxed ((!))
import Data.List          ((\\))

import Data.Accessor.Basic ((^.), (^=), (^:))

import External.Graphics.Rendering (renderScene)
import External.Time               (resetTimer, readTimer, sleep)
import Game.Activity               (Activity(..))
import Game.Entity.Dot             (Dot)
import Game.Entity.Dot.Activity    (Activity(..))
import Game.G                      (G, ask, get, getRandomR, put)
import Game.Level                  (Level(..), numDots, numDotsRequired)
import Game.Score                  (Score(..))
import qualified External.Input.Keyboard      as Keyboard
import qualified External.Input.Keyboard.Keys as Keyboard
import qualified External.Input.Mouse         as Mouse
import qualified External.Input.Mouse.Buttons as Mouse
import qualified Game.Entity.Dot              as Dot
import qualified Game.Entity.Dot.Radius       as Radius
import qualified Game.Environment             as Environment
import qualified Game.Level                   as Level
import qualified Game.Score                   as Score
import qualified Game.State                   as State
import qualified Graphics.Color               as Color
import qualified Space.Displacement2          as Displacement2
import qualified Space.Position2              as Position2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

logic :: G ()
logic = do
    resetTimer

    e <- ask
    s <- get

    q0 <- Keyboard.pressed Keyboard.escape
    q1 <- Keyboard.pressed Keyboard.q
    if q0 || q1
      then put $ State.activity ^= Quitting $ s
      else do
          case s^.State.activity of
            Starting ->
                put $ State.activity ^= PreparingLevel Level1 $
                      State.score    ^= NoScore               $ s

            PreparingLevel level ->
                let nd  = numDots         level
                    ndr = numDotsRequired level
                    np  = if s^.State.score == NoScore then 0 else s^.State.score^.Score.numPoints
                in do
                    dots <- makeDots nd
                    put $ State.score    ^= Score nd ndr 0 np  $
                          State.activity ^= Playing level dots $ s

            Playing level dots -> do
                dots' <- updateDots dots
                mxy   <- Mouse.clicked Mouse.left
                case mxy of
                  Nothing ->
                    put $ State.activity ^= Playing level dots' $ s
                  Just (x,y) ->
                    let radius       = e^.Environment.normalDotRadius
                        color        = e^.Environment.placedDotColor
                        position     = Position2.make x y
                        displacement = Displacement2.make (0 :: Double) (0 :: Double)
                        activity     = Expanding (e^.Environment.expandingDotA)
                        dot          = Dot.make radius color position displacement activity
                    in put $ State.activity ^= Colliding level [dot] dots' $ s

                renderScene

            Colliding level [] dots ->
                put $ State.activity ^= activity' $ s
              where
                activity' = EndingLevel level dots'
                dots'     = map (Dot.activity ^= Ending (e^.Environment.endingDotA)) dots

            Colliding level activeDots roamingDots -> do
                activeDots'  <- updateDots activeDots
                roamingDots' <- updateDots roamingDots
                (activeDots'', roamingDots'') <- activate activeDots' roamingDots'

                let a'  = Colliding level activeDots'' roamingDots''
                let p   = fromIntegral (length activeDots'' - length activeDots')
                let sc  = s^.State.score
                let sc' = Score.numDotsActivated ^: (p +) $ sc

                put $ State.activity ^= a'  $
                      State.score    ^= sc' $ s

                renderScene

            EndingLevel level dots -> do
                dots' <- updateDots dots
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

            Quitting ->
                return ()

          t <- fmap ((e^.Environment.secondsPerFrame) -) readTimer
          when (t > 0) (sleep t)

          when (s^.State.activity /= Quitting) logic

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

activate :: [Dot Double] -> [Dot Double] -> G ([Dot Double],[Dot Double])
activate activeDots roamingDots = do
    let newActiveDots = [r | a <- activeDots, r <- roamingDots, areColliding a r]
    let roamingDots' = roamingDots \\ newActiveDots

    e <- ask
    let a' = Expanding (e^.Environment.expandingDotA)
    let activeDots' = activeDots ++ map (Dot.activity ^= a') newActiveDots

    return (activeDots',roamingDots')
  where
    areColliding d0 d1 =
        xDistance < maxDistance &&
        yDistance < maxDistance &&
        sqrt (xDistance^(2 :: Int) + yDistance^(2 :: Int)) < maxDistance
      where
        xDistance = abs (d0x - d1x)
        yDistance = abs (d0y - d1y)

        d0x = d0p^.Position2.x
        d0y = d0p^.Position2.y
        d0p = d0^.Dot.position

        d1x = d1p^.Position2.x
        d1y = d1p^.Position2.y
        d1p = d1^.Dot.position

        maxDistance = r0 + r1
        r0 = d0^.Dot.radius^.Radius.value
        r1 = d1^.Dot.radius^.Radius.value

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

makeDots :: Integer -> G [Dot Double]
makeDots n =
    replicateM (fromIntegral n) makeDot

makeDot :: G (Dot Double)
makeDot = do
    e <- ask
    let ac   = e^.Environment.initialDotAlpha
    let r    = e^.Environment.normalDotRadius
    let mind = e^.Environment.minDotDisplacement
    let maxd = e^.Environment.maxDotDisplacement

    rc <- randomDoubleR 0 1
    gc <- randomDoubleR 0 1
    bc <- randomDoubleR 0 1

    px <- randomDoubleR (-0.9) 0.9
    py <- randomDoubleR (-0.9) 0.9

    dx_ <- randomDoubleR mind maxd
    dy_ <- randomDoubleR mind maxd
    dx <- fmap (\b -> if b then negate dx_ else dx_) randomBool
    dy <- fmap (\b -> if b then negate dy_ else dy_) randomBool

    let radius       = r
    let color        = Color.make rc gc bc ac
    let position     = Position2.make px py
    let displacement = Displacement2.make dx dy
    let activity     = Appearing (e^.Environment.appearingDotA)

    return $ Dot.make radius color position displacement activity
  where
    randomDoubleR l h = getRandomR (l,h) :: G Double
    randomBool        = fmap (== 0) (getRandomR (0,1) :: G Int)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

updateDots :: [Dot Double] -> G [Dot Double]
updateDots =
    fmap (filter isAlive) . mapM updateDot
  where
    isAlive d = d^.Dot.activity /= None

updateDot :: Dot Double -> G (Dot Double)
updateDot dot =
    case dot^.Dot.activity of
      Appearing   n -> updateAppearingDot   dot n
      Roaming       -> updateRoamingDot     dot
      Expanding   n -> updateExpandingDot   dot n
      Holding     n -> updateHoldingDot     dot n
      Contracting n -> updateContractingDot dot n
      Ending      n -> updateEndingDot      dot n
      None          -> return dot

updateAppearingDot :: Dot Double -> Integer -> G (Dot Double)
updateAppearingDot dot n =
    ask                          >>= \e    ->
    updateRoamingDotPosition dot >>=
    updateRoamingDotDisplacement >>= \dot' ->
    return $
      if n == e^.Environment.appearingDotZ
        then let r' = e^.Environment.normalDotRadius
                 a' = Roaming
             in Dot.radius   ^= r' $
                Dot.activity ^= a' $ dot'
        else let r' = Radius.make ((e^.Environment.appearingDotRadii) ! n)
                 a' = Appearing (succ n)
             in Dot.radius   ^= r' $
                Dot.activity ^= a' $ dot'

updateRoamingDot :: Dot Double -> G (Dot Double)
updateRoamingDot dot =
    updateRoamingDotPosition dot >>=
    updateRoamingDotDisplacement

updateExpandingDot :: Dot Double -> Integer -> G (Dot Double)
updateExpandingDot dot n =
    ask >>= \e ->
    return $
      if n == e^.Environment.expandingDotZ
        then let a' = Holding (e^.Environment.holdingDotA)
             in Dot.activity ^= a' $ dot
        else let r' = Radius.make ((e^.Environment.expandingDotRadii) ! n)
                 a' = Expanding (succ n)
             in Dot.radius   ^= r' $
                Dot.activity ^= a' $ dot

updateHoldingDot :: Dot Double -> Integer -> G (Dot Double)
updateHoldingDot dot n = do
    e <- ask
    return $
      if n == e^.Environment.holdingDotZ
        then let a' = Contracting (e^.Environment.contractingDotA)
             in Dot.activity ^= a' $ dot
        else let r' = Radius.make ((e^.Environment.holdingDotRadii) ! n)
                 a' = Holding (succ n)
             in Dot.radius   ^= r' $
                Dot.activity ^= a' $ dot

updateContractingDot :: Dot Double -> Integer -> G (Dot Double)
updateContractingDot dot n = do
    e <- ask
    return $
      if n == e^.Environment.contractingDotZ
        then Dot.activity ^= None $ dot
        else let r' = Radius.make ((e^.Environment.contractingDotRadii) ! n)
                 a' = Contracting (succ n)
             in Dot.radius   ^= r' $
                Dot.activity ^= a' $ dot

updateEndingDot :: Dot Double -> Integer -> G (Dot Double)
updateEndingDot dot n = do
    e <- ask
    return $
      if n == e^.Environment.endingDotZ
        then Dot.activity ^= None $ dot
        else let r' = Radius.make ((e^.Environment.endingDotRadii) ! n)
                 a' = Ending (succ n)
             in Dot.radius   ^= r' $
                Dot.activity ^= a' $ dot

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

updateRoamingDotPosition :: Dot Double -> G (Dot Double)
updateRoamingDotPosition dot =
    return $ Dot.position ^= p' $ dot
  where
    p' = Position2.displace p d
    p  = dot^.Dot.position
    d  = dot^.Dot.displacement

updateRoamingDotDisplacement :: Dot Double -> G (Dot Double)
updateRoamingDotDisplacement dot =
    let p   = dot^.Dot.position
        px  = p^.Position2.x
        py  = p^.Position2.y

        d   = dot^.Dot.displacement
        dx  = d^.Displacement2.x
        dy  = d^.Displacement2.y

        rv  = dot^.Dot.radius^.Radius.value

        dx' = if px < n1 + rv || px > p1 - rv then negate dx else dx
        dy' = if py < n1 + rv || py > p1 - rv then negate dy else dy
        d'  = Displacement2.make dx' dy'
    in return $ Dot.displacement ^= d' $ dot

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

n1, p1 :: Double
n1 = -1
p1 =  1
