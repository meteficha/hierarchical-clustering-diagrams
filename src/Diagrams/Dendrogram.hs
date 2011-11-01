{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Dendrogram
    ( dendrogram
    ) where

-- from base
import Control.Arrow (second)

-- from hierarachical-clustering
import Data.Clustering.Hierarchical (Dendrogram(..), elements)

-- from diagrams-lib
import Diagrams.Prelude


-- | @dendrogram drawItem dendro@ is a drawing of the dendrogram
-- @dendro@ using @drawItem@ to draw its leafs.
dendrogram :: (Monoid m, Renderable (Path R2) b) =>
              (a -> AnnDiagram b R2 m)
              -> Dendrogram a
              -> AnnDiagram b R2 m
dendrogram drawItem dendro = (stroke path_ # value mempty) === items
  where
    drawItems d = hcat [drawItem a # alignT # named n | (a, n) <- elements d]

    dendroPath (Leaf (_, n))  = (mempty, getPos n)
    dendroPath (Branch d l r) = (path <> pathL <> pathR, pos)
        where
          (pathL, P (xL, yL)) = dendroPath l
          (pathR, P (xR, yR)) = dendroPath r

          path = fromVertices [ P (xL, yL)
                              , P (xL, d+y0)
                              , P (xR, d+y0)
                              , P (xR, yR)]
          pos  = P ((xL + xR) / 2, d+y0)

    named_     = nameDendrogram dendro
    items      = drawItems named_
    names_     = names items
    getPos n   = let Just [(p, _)] = lookupN n names_ in p
    (path_, _) = dendroPath named_

    Just [(P (_, y0), _)] = lookupN (0 :: Int) names_

nameDendrogram :: Dendrogram a -> Dendrogram (a, Int)
nameDendrogram = snd . go 0
    where
      go n (Leaf a) = n `seq` (n+1, Leaf (a,n))
      go n (Branch d l r) =
          let (n', l') = go n l
          in second (Branch d l') (go n' r)
