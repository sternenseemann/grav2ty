{-# LANGUAGE TemplateHaskell #-}
module Grav2ty.Simulation
  (
  -- * Objects
    realHitbox
  , updateObject
  , gravitationForce
  -- * Hitboxes
  , translateHitbox
  , rotateHitbox
  , collision
  , objectCollision
  -- * Object Relations
  , ObjRelGraph (..)
  , ObjRel (..)
  , objectRelGraph
  -- * Exposed Utilities
  , rotateV2
  ) where

import Grav2ty.Core
import Grav2ty.Util.RelGraph

import Control.Lens
import Data.Complex
import Data.Foldable
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as M
import Linear.Matrix
import Linear.Metric (norm, distance)
import Linear.V2
import Linear.Vector

-- | @translateHitbox (V2 x y)@ translates a 'Hitbox' by (x, y) in a 2d plane.
translateHitbox :: Num a => V2 a -> Hitbox a -> Hitbox a
translateHitbox t (HLine a b) = HLine (a + t) (b + t)
translateHitbox t (HCircle c r) = HCircle (c + t) r
translateHitbox t (HCombined hs) = HCombined . map (translateHitbox t) $ hs

complexV2 :: Iso' (Complex a) (V2 a)
complexV2 = iso (\(x :+ y) -> V2 x y) (\(V2 x y) -> x :+ y)

-- | Rotate a point by an radial angle around @V2 0 0@ using 'Complex' multiplication.
--   When using this rotation make sure to rotate before translating.
rotateV2 :: RealFloat a => a -> V2 a -> V2 a
rotateV2 angle = (^. complexV2) . (* rotator) . (^. from complexV2)
  where rotator = cos angle :+ sin angle

-- TODO address inaccuracies of 'Float' and 'Double'?
-- | @rotateHitbox@ rotates a 'Hitbox' by the given angle
--   which must be radial. As it uses 'Complex' multiplication
--   the center of the rotation is always @V2 0 0@, so make
--   sure to rotate before translating a 'Hitbox'.
rotateHitbox :: RealFloat a => a -> Hitbox a -> Hitbox a
rotateHitbox angle box =
  case box of
    HLine a b -> HLine (rotateV2 angle a) (rotateV2 angle b)
    HCircle c r -> HCircle (rotateV2 angle c) r
    HCombined l -> HCombined . map (rotateHitbox angle) $ l

-- | Returns the 'Hitbox' for an 'Object' — rotated and translated
--   to the location it is *actually* at.
realHitbox :: RealFloat a => Object a -> Hitbox a
realHitbox obj = translateHitbox (objectLoc obj) . rotateHitbox (objectRot obj)
  . objectHitbox $ obj

-- | Implementation of
--   [Cramer's rule](https://en.wikipedia.org/wiki/Cramer%27s_rule) for solving
--   a system of two linear equations.
cramer2 :: (Eq a, Fractional a) => M22 a -> V2 a -> Maybe (a, a)
cramer2 coeff res = if detA == 0
                      then Nothing
                      else Just (det22 a1 / detA, det22 a2 / detA)
  where detA = det22 coeff
        a1 = set (column _x) res coeff
        a2 = set (column _y) res coeff

-- | Wether a value is between two other values.
inRange :: Ord a => (a, a) -> a -> Bool
inRange (l, u) x = x >= l && x <= u

-- | Determines wether two 'Hitbox'es collide, i. e. any individual
--   'HCircle' or 'HLine' intersect.
--
--   * Intersection of two 'HCircle' hitboxes is achieved by substracting the
--     sum of their radius from the (absolute) distance of their centers.
--     If this number is 0 or negative, the circles touch or intersect.
--   * Intersection of a 'HLine' and a 'HCircle' is calculated by solving the
--     equation describing the intersection of the infinite line the 'HLine' is
--     on and the 'HCircle'. After that we check, wether the possible
--     intersection points are on the 'HLine'. More details on the equation
--     and the origins of its derivation are found in my notes which can be
--     found in @doc/intersection-line-circle.pdf@ of the source distribution of
--     this package.
--   * If 'HCombined' is involved, all possible intersections are recursively
--     checked. This might be an area of future bugs.
--
--   Note that in calculation we rely on Laziness to prevent DivByZero
--   Exceptions.
collision :: (Ord a, Floating a) => Hitbox a -> Hitbox a -> Bool
collision (HCircle l1 r1) (HCircle l2 r2) = distance l1 l2 - r1 - r2 <= 0
collision (HLine start end) (HCircle (V2 cx cy) r) =
  let (V2 dirX dirY) = end - start
      (V2 ax ay) = start
      a = dirX ^ 2 + dirY ^ 2
      b = 2 * dirX * ax + 2 * dirY * ay - 2 * cx * dirX - 2 * cy * dirY
      c = ax ^ 2 + ay ^ 2 - 2 * cx * ax - 2 * cy * ay + cx ^ 2 + cy ^ 2 - r ^ 2
      discriminant = b ** 2 - 4 * a * c
      solution m = (-b `m` sqrt discriminant) / (2 * a)
      solutions = solution (+) : [solution (-) | discriminant > 0]
   in discriminant >= 0 -- there is a possible intersection
   && a /= 0 -- HLine is proper line (i.e. has distinct start and end points)
   && any (inRange (0, 1)) solutions
   -- at least one solutions is actually on the line. the line is modeled as
   -- p = start + q * dir where dir = end - start. therefore all points p with
   -- 0 <= q <= 1 are on the line.
collision a@(HCircle _ _) b@(HLine _ _) = collision b a
-- TODO collision not registered if the lines are parallel, but touch each
-- other (Nothing is returned).
collision (HLine a1 b1) (HLine a2 b2) =
  let (V2 xd1 yd1) = b1 - a1
      (V2 xd2 yd2) = b2 - a2
      (V2 xa1 ya1) = a1
      (V2 xa2 ya2) = a2
      coeff = V2 (V2 xd1 (-xd2)) (V2 yd1 (-yd2))
      res = V2 (xa2 - xa1) (ya2 - ya1)
      solutions = cramer2 coeff res
   in case solutions of
        Just (s, t) -> inRange (0, 1) s && inRange (0, 1) t
        Nothing -> False
collision (HCombined as) b = any (collision b) as
collision a b@(HCombined _) = collision b a

-- | Wether two given 'Object's collide.
objectCollision :: RealFloat a => Object a -> Object a -> Bool
objectCollision a b = a /= b &&
  ((objectLoc a == objectLoc b)
    || collision (realHitbox a) (realHitbox b))

-- | Wether two 'Object's have a certain distance. Used to
--   prevent extreme spikes in gravitational force while simulating
--   two objects that are close together.
separated :: (Floating a, Ord a) => Object a -> Object a -> Bool
separated a b = distance (objectLoc a) (objectLoc b) > 3

-- | Directional Gravitational Force between two Objects as two-dimensional vector.
gravitationForce :: Floating a => Object a -> Object a -> V2 a
gravitationForce a b = (gravityConst *
  ((objectMass a * objectMass b) / (absDistance ** 2)))
  *^ (distance ^/ absDistance)
  where gravityConst = 6.67408e-11
        distance = objectLoc b - objectLoc a
        absDistance = norm distance

-- | The sum of gravitational force the 'World' imposes on an 'Object'.
gravitationForces :: (Ord a, Floating a) => World a -> Object a -> V2 a
gravitationForces world obj = foldl' calcSum (pure 0) world
  where calcSum force x = if separated obj x
                             then force + gravitationForce obj x
                             else force

data ObjRel a = ObjRel
  { _relColl  :: Bool -- ^ Wether the two 'Object's collide
  , _relForce :: V2 a -- ^ The gravitation force between them
  } deriving (Show, Eq, Ord)

makeLenses 'ObjRel

-- | The Object Relation Graph is used to store collisions
--   and the forces between 'Object's. This prevents that
--   these values are computed more than once and allows
--   to reuse computations for the inverse relation.
type ObjRelGraph a = RelGraph Id (ObjRel a)

-- | Calculates all 'ObjRel's for a 'World'.
objectRelGraph :: (RealFloat a, Ord a) => World a -> ObjRelGraph a
objectRelGraph = insertMapKey rel emptyRel
  where rel a b = let res = ObjRel (objectCollision a b) (gravity a b)
                   in (res, over relForce negated res)
        gravity a b = if separated a b
                         then gravitationForce a b
                         else pure 0

-- | @updateObject timeStep gravitionForce@ calculates the state of
--   an 'Object' after a certain @timeStep@ of being exposed to
--   a certain @gravitationForce@. It also factors in the current
--   acceleration and speed of the 'Object'.
updateObject :: Fractional a => a -> V2 a -> Object a -> Object a
updateObject _ _ obj@Static {} = obj
updateObject timeStep force obj@Dynamic {} = obj
    { objectLoc   = objectLoc obj + (objectSpeed obj ^* timeStep)
    , objectSpeed = objectSpeed obj +
      (((force ^/ objectMass obj) + objectAcc obj) ^* timeStep)
    }

