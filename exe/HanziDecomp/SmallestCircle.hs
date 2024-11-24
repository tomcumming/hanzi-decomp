module HanziDecomp.SmallestCircle
  ( Circle (..),
    isInsideCircle,
    smallestCircle,
  )
where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Diagrams qualified as D
import HanziDecomp.Maths (V2, dot, mag, unit)

data Circle = Circle
  { cirCenter :: V2,
    cirRadius :: Double
  }
  deriving (Show)

smallestCircle :: NE.NonEmpty V2 -> Circle
smallestCircle ps =
  welzls (toList ps) []
    & fromMaybe Circle {cirCenter = NE.head ps, cirRadius = 0}

welzls :: [V2] -> [V2] -> Maybe Circle
welzls ps rs = case ps of
  _ | [p1, p2, p3] <- rs -> circleFromThreePoints p1 p2 p3 & Just
  [] -> trivial rs
  (p : ps') ->
    let d = welzls ps' rs
     in if maybe False (isInsideCircle p) d
          then d
          else welzls ps' (p : rs)

trivial :: [V2] -> Maybe Circle
trivial = \case
  [] -> Nothing
  [cirCenter] -> Circle {cirCenter, cirRadius = 0} & Just
  [p1, p2] -> circleFromTwoPoints p1 p2 & Just
  [p1, p2, p3] -> circleFromThreePoints p1 p2 p3 & Just
  _ -> error "trivial > 3"

isInsideCircle :: V2 -> Circle -> Bool
isInsideCircle p Circle {..} = mag (cirCenter - p) <= cirRadius

circleFromTwoPoints :: V2 -> V2 -> Circle
circleFromTwoPoints p1 p2 =
  Circle
    { cirCenter = p1 + (p2 - p1) / 2,
      cirRadius = p2 - p1 & mag & (/ 2)
    }

circleFromThreePoints :: V2 -> V2 -> V2 -> Circle
circleFromThreePoints a b c = Circle {..}
  where
    abo = a + (b - a) / 2
    abn = b - a & unit

    bco = b + (c - b) / 2
    bcd = let D.V2 x y = c - b in D.V2 y (-x) & unit

    d = dot abn (abo - bco)
    t = d / dot bcd abn

    cirCenter = bco + D.scale t bcd
    cirRadius = subtract a cirCenter & mag
