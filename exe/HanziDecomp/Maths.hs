module HanziDecomp.Maths
  ( V2,
    Line,
    mag,
    unit,
    dot,
    scale,
    lineLength,
    lineCentroid,
    linesCentroid,
    bestPlane,
    pointsToLines,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Diagrams qualified as D

type V2 = D.V2 Double

type Line = (V2, V2)

-- | Because D.scale doesn't like zeros
scale :: Double -> V2 -> V2
scale k (D.V2 x y) = D.V2 (k * x) (k * y)

mag :: V2 -> Double
mag v = dot v v & sqrt

-- | Beware small inputs
unit :: V2 -> V2
unit v = scale (1 / mag v) v

dot :: V2 -> V2 -> Double
dot (D.V2 x1 y1) (D.V2 x2 y2) = x1 * x2 + y1 * y2

lineLength :: Line -> Double
lineLength (st, en) = en - st & mag

lineCentroid :: Line -> V2
lineCentroid (st, en) = st + scale (1 / 2) (en - st)

linesCentroid :: NE.NonEmpty Line -> V2
linesCentroid ls =
  fmap
    (\l -> lineCentroid l & scale (lineLength l / totalLength))
    ls
    & sum
  where
    totalLength = fmap lineLength ls & sum

-- TODO check how wrong
bestPlane ::
  V2 ->
  NE.NonEmpty Line ->
  Double
bestPlane c ls =
  fmap
    (\l -> linePlane l * (lineLength l / totalLength))
    ls
    & sum
  where
    totalLength = fmap lineLength ls & sum

    linePlane :: Line -> Double
    linePlane =
      lineCentroid
        >>> (`subtract` c)
        >>> (\(D.V2 x y) -> atan2 y x)
        >>> (+ pi)
        >>> (\a -> a - realToFrac @Int (floor (a / pi)) * pi)

pointsToLines :: [V2] -> Maybe (NE.NonEmpty Line)
pointsToLines = \case
  (st : en : rest) -> (st, en) NE.:| go (en : rest) & Just
  _ -> Nothing
  where
    go = \case
      (st : en : rest) -> (st, en) : go (en : rest)
      _ -> []
