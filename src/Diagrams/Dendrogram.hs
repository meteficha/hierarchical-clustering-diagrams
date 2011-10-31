{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Diagrams.Dendrogram
    ( dendrogram
    ) where

-- from base
import Data.Typeable (Typeable)

-- from hierarachical-clustering
import Data.Clustering.Hierarchical (Dendrogram(..))

-- from diagrams-lib
import Diagrams.Prelude


-- | @dendrogram drawItem dendro@ is a drawing of the dendrogram
-- @dendro@ using @drawItem@ to draw its leafs.
dendrogram :: Renderable (Path R2) b =>
              (t -> AnnDiagram b R2 Any)
           -> Dendrogram t
           -> AnnDiagram b R2 Any
dendrogram drawItem = lineCap LineCapSquare . lw 0.1 . fst . drawDendro
  where
    drawDendro (Leaf a) = (dia, 0)
        where
          dia = drawItem a # alignT
    drawDendro (Branch d l r) = (dia, d)
        where
          (diaL, posL) = drawDendro l
          (diaR, posR) = drawDendro r

          line pos name = vrule (d - pos) # alignT # named name
          diaL' = line posL L === (D |> diaL)
          diaR' = line posR R === (D |> diaR)
          horizLine = withName L $ \(pl, _) ->
                      withName R $ \(pr, _) ->
                          stroke (pl ~~ pr) # atop

          dia = horizLine (diaL' # alignT ||| diaR' # alignT)

data Discard = D deriving (Typeable, Show, Ord, Eq)
instance IsName Discard where

data ConnectingPoint = L | R deriving (Typeable, Show, Ord, Eq)
instance IsName ConnectingPoint where
