module Main where

import Control.Lens
import Debug.Trace
import Linear.Metric (norm, distance)
import Linear.V2
import Linear.Vector

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

fps :: Num a => a
fps = 200

data Hitbox a
  = HLine
  { lineStart  :: V2 a
  , lineEnd    :: V2 a
  }
  | HMultiple [Hitbox a]
  | HCircle
  { circleLoc    :: V2 a
  , circleRadius :: a
  } deriving (Eq, Show, Ord)

data Object a
  = Dynamic
  { objectHitbox :: Hitbox a  -- ^  hitbox of the object. Hitbox points at
                              --    (V2 0 0) will always be at the center of the
                              --    object
  , objectRot    :: a         -- ^ Radial angle
  , objectMass   :: a         -- ^ mass of the object in kg
  , objectLoc    :: V2 a      -- ^ Current location of the object.
  , objectSpeed  :: V2 a      -- ^ Current speed of the Object. Used for
                              --   simulation approximation
  , objectAcc    :: V2 a      -- ^ Current static Acceleration of the object.
                              --   0 unless controlled by the player or
                              --   projectile.
  , objectPlayer :: Bool      -- ^ Wether the object is controlled by the player
  }
  | Static
  { objectHitbox :: Hitbox a
  , objectRot    :: a
  , objectMass   :: a
  , objectLoc    :: V2 a
  } deriving (Show, Eq, Ord)

isStatic :: Object a -> Bool
isStatic Static {} = True
isStatic _         = False

isDynamic :: Object a -> Bool
isDynamic Dynamic {} = True
isDynamic _          = False

gravitationForce :: Floating a => Object a -> Object a -> V2 a
gravitationForce a b = (gravityConst *
  ((objectMass a * objectMass b) / (absDistance ** 2)))
  *^ (distance ^/ absDistance)
  where gravityConst = 6.67408e-11
        distance = objectLoc b - objectLoc a
        absDistance = norm distance

separated :: (Floating a, Ord a) => Object a -> Object a -> Bool
separated a b = distance (objectLoc a) (objectLoc b) > 3

gravitationForces :: (Ord a, Floating a) => [Object a] -> Object a -> [V2 a]
gravitationForces []     _   = []
gravitationForces (w:ws) obj =
  if separated obj w
     then gravitationForce obj w : gravitationForces ws obj
     else gravitationForces ws obj

updateObject :: Fractional a =>  a -> [V2 a] -> Object a -> Object a
updateObject _ _ obj@Static {} = obj
updateObject timeStep forces obj@Dynamic {} = obj
    { objectLoc   = objectLoc obj + (objectSpeed obj ^* timeStep)
    , objectSpeed = objectSpeed obj +
      (((sum forces ^/ objectMass obj) + objectAcc obj) ^* timeStep)
    }

applyPlayerState :: Floating a => PlayerState a -> Object a -> Object a
applyPlayerState _ obj@Static {} = obj
applyPlayerState _ obj@Dynamic { objectPlayer = False} = obj
applyPlayerState ps obj@Dynamic { objectPlayer = True} = obj
  { objectRot = shipRot ps
  , objectAcc = angle (shipRot ps) ^* shipAcc ps
  }

vectorToPoint :: V2 a -> (a, a)
vectorToPoint (V2 x y) = (x, y)

renderObject :: Object Float -> Picture
renderObject obj = translate x y . rot . renderHitbox . objectHitbox $ obj
  where (V2 x y)  = objectLoc obj
        rot       =  rotate (clockwise . toDegree . objectRot $ obj)
        toDegree  = (*) (360 / (2 * pi))
        clockwise = (*) (-1)

renderHitbox :: Hitbox Float -> Picture
renderHitbox box =  Color white $
  case box of
    HCircle (V2 x' y') r -> translate x' y' $ Circle r
    HLine a b -> Line . map vectorToPoint $ [a, b]
    HMultiple boxes -> Pictures $ map renderHitbox boxes

centeredCircle :: Num a => a -> Hitbox a
centeredCircle r = HCircle (V2 0 0) r

data PlayerState a
  = PlayerState
  { shipRot :: a -- ^ Radial angle the ship is rotated at.
  , shipAcc :: a -- ^ Current acceleration of the ship.
  } deriving (Show, Eq)

data State a
  = State
  { player :: PlayerState a
  , world  :: [Object a]
  } deriving (Show, Eq)

shipHitbox :: Num a => Hitbox a
shipHitbox = HMultiple
  [ HLine (V2 (-10) (-5)) (V2 (-10) 5)
  , HLine (V2 (-10) (-5)) (V2 10 0)
  , HLine (V2 (-10) 5)    (V2 10 0)
  ]

initialWorld :: Fractional a => State a
initialWorld = State (PlayerState 0 0)
  [ Dynamic shipHitbox 0 10000 (V2 130 20) (V2 0 0) (V2 0 0) True
  , Static (centeredCircle 80) 0 moonMass (V2 0 0)
--  , Static (centeredCircle 40) 0 (0.5 * moonMass) (V2 250 120)
  ]
  where moonMass = 8e13

renderUi :: Show a => PlayerState a -> Picture
renderUi ps = translate (-350) (350) . scale 0.3 0.3 . Color green . Text . show $ shipAcc ps

renderWorld :: State Float -> Picture
renderWorld (State ps world) = Pictures
  . (:) (renderUi ps)
  . map renderObject $ world

updateWorld :: Float -> State Float -> State Float
updateWorld timeStep (State ps world) = State ps $ map
  (\obj ->
    updateObject timeStep (gravitationForces world obj) .
      applyPlayerState ps $ obj)
  world

eventHandler :: Floating a => Event -> State a -> State a
eventHandler (EventKey key Down _ _) state = state
  { player = case key of
               SpecialKey KeyUp -> (player state) { shipAcc = shipAcc (player state) + 1 }
               SpecialKey KeyDown -> (player state) { shipAcc = shipAcc (player state) - 1 }
               SpecialKey KeyLeft -> (player state) { shipRot = shipRot (player state) + 0.1 }
               SpecialKey KeyRight -> (player state) { shipRot = shipRot (player state) - 0.1 }
               _                   -> player state
  }
eventHandler _ s = s

main :: IO ()
main = play
  (InWindow "grav2ty" (800, 800) (0,0))
  black
  fps
  initialWorld
  renderWorld
  eventHandler
  updateWorld
