module HanziDecomp.Convex (convexHull) where

import Control.Category ((>>>))
import Data.Foldable (toList)
import Data.Foldable1 (maximum, minimum)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Diagrams qualified as D
import HanziDecomp.Maths (V2, dot, mag, scale)
import Prelude hiding (maximum, minimum)

-- | Calculate convex hull in rendering order
convexHull :: NE.NonEmpty V2 -> NE.NonEmpty V2
convexHull ps
  | NE.length ps < 3 = ps
  | otherwise =
      pointsAround c ps
        & simplify
        & M.toList
        & NE.fromList -- should be safe...
        & fmap (uncurry relativePoint >>> (c +))
  where
    c = middle ps

relativePoint :: Double -> Double -> V2
relativePoint a m = D.V2 (cos a) (sin a) & scale m

indexPoint :: V2 -> (Double, Double)
indexPoint p@(D.V2 x y) = (atan2 y x, mag p)

pointsAround :: V2 -> NE.NonEmpty V2 -> M.Map Double Double
pointsAround c =
  fmap (subtract c >>> indexPoint)
    >>> toList
    >>> M.fromListWith max

simplify :: M.Map Double Double -> M.Map Double Double
simplify = simplify' mempty

simplify' :: M.Map Double Double -> M.Map Double Double -> M.Map Double Double
simplify' solved pending = case M.minViewWithKey pending of
  Nothing -> solved
  Just ((a, m), pending')
    | redundant dirIn dirOut -> simplify' (M.insert a m solved) pending'
    | otherwise -> backtrack solved pending'
    where
      (na, nm) = M.lookupMin pending' & fromMaybe (M.findMin solved)
      (pa, pm) = M.lookupMax solved & fromMaybe (M.findMax pending)
      dirIn = relativePoint a m - relativePoint pa pm
      dirOut = relativePoint na nm - relativePoint a m
  where
    redundant :: V2 -> V2 -> Bool
    redundant (D.V2 x y) dirOut = left `dot` dirOut <= 0
      where
        left = D.V2 y (-x)

backtrack :: M.Map Double Double -> M.Map Double Double -> M.Map Double Double
backtrack solved pending = case M.maxViewWithKey solved of
  Nothing -> simplify' mempty pending
  Just ((a, m), solved') -> simplify' solved' (M.insert a m pending)

middle :: NE.NonEmpty V2 -> V2
middle ps = D.V2 (xmin + (xmax - xmin) / 2) (ymin + (ymax - ymin) / 2)
  where
    xs = fmap (\(D.V2 x _) -> x) ps
    ys = fmap (\(D.V2 _ y) -> y) ps
    xmax = maximum xs
    xmin = minimum xs
    ymax = maximum ys
    ymin = minimum ys
