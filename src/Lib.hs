{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)


type Node = Int

-- | A map from in-nodes to out-nodes.
type OutEdges = Map Node (Set Node)

-- | A reversed list of `Node`s connected by out-edges.
-- That is, the chain 0 -> 1 -> 2 is represented as [2,1,0].
-- (That reversal avoids the need to dig into the list to extend it.)
type Chain = [Node]

-- | A graph is a set of nodes and edges.
-- Keeping the nodes as a set seems natural,
-- so that we can quickly (in time 0(1))
-- test whether a given node is a member of the graph.
-- Using `OutEdges` to represent edges allows us to quickly
-- (in time O(out-neighbors)) find all out-neighbors of a given node.
data Graph = Graph {
    _gNodes :: Set Node
  , _gOutEdges :: OutEdges -- ^ Edges are ordered.
    -- It is possible that x considers y a friend, but not vice-versa.
    -- If every edge should be accompanied by its dual,
    -- use `symmetricGraphFromEdges`, below.
  } deriving (Show, Eq, Ord)
makeLenses ''Graph


-- | `lengthOfShortestChain (start,end) g` returns the length of the
-- shortest chain from `start` to `end` in `g`,
-- or `Left` if there is no such chain.
--
-- This is breadth-first search (BFS). I considered DFS also,
-- but BFS has better speed properties.
-- DFS could send you down needless rabbit holes,
-- searching for a long time when there's a much shorter path.
-- BFS is guaranteed to find the result in a number of iterations
-- equal to the length of the shortest chain, if one exists.
--
-- Algorithm:
-- Start with the singleton chain `[start]`.
-- For each chain, construct all relevant chains that are one node longer.
-- If any of the new chains begins with `end`, return `count`.
-- Otherwise, delete every edge that was used, increment count, and repeat.

lengthOfShortestChain :: (Node,Node) -> Graph -> Either String Int
lengthOfShortestChain (start,end) g0 =
  if start == end
     then if elem start $ _gNodes g0
          then Right 0
          else noSuchChain
     else go [[start]] 1 g0
  where
    noSuchChain = Left $ "No path leads from "
                  ++ show start ++ " to " ++ show end ++ "."
    go :: [Chain] -> Int -> Graph -> Either String Int
    go cs count g = let
      cs' :: [Chain]
      cs' = concatMap (extendChain g) cs
      in if S.member end $ S.fromList $ map head cs'
         then Right count
      else if length cs' == 0
           then noSuchChain
           else let
      deleteOutEdges :: Node -> Graph -> Graph
      deleteOutEdges n = gOutEdges %~ M.delete n
      g' :: Graph
      g' = foldr deleteOutEdges g $ map head cs
      in go cs' (count+1) g'

-- | `extendChain g c@(h:_)` returns a list of all `Chain`s
-- that can be constructed by prepending a friend of `h` to `c`.
extendChain :: Graph -> Chain -> [Chain]
extendChain _ [] = error "this should not happen"
extendChain g c@(h:_) = case M.lookup h $ _gOutEdges g of
  Nothing -> []
  Just friends -> map (:c) $ S.toList friends

-- | `graphFromEdges` constructs a graph from a list of edges.
-- A graph that includes people who are members of no friend relationships
-- cannot be represented this way. That seems fine for our purposes,
-- because isolated nodes are of no interest when finding chains.
graphFromEdges :: [(Node,Node)] -> Graph
graphFromEdges es =
  let pairToList :: (Node,Node) -> [Node]
      pairToList (f,g) = [f,g]
      nodes :: Set Node
      nodes = S.fromList $ concatMap pairToList es
      mkEdges :: [(Node,Node)] -> OutEdges
      mkEdges = foldr f mempty where
        f :: (Node,Node) -> OutEdges -> OutEdges
        f (x,y) = M.insertWith S.union x $ S.singleton y
  in Graph nodes $ mkEdges es

symmetricGraphFromEdges :: [(Node,Node)] -> Graph
symmetricGraphFromEdges es =
  graphFromEdges $ es ++ map (\(x,y) -> (y,x)) es
