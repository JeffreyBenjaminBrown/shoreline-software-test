{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Map as M
import           Data.Map (Map)


type Node = Int
type OutEdges = Map Node (Set Node)
type Chain = [Int]

-- | This data structure seemed like it would be fast for the desired operations.
-- Finding a node's out-neighbors is O(n) where n is the number of neighbors.
data Graph = Graph {
    _gNodes :: Set Node
  , _gEdges :: OutEdges -- ^ Edges are ordered.
    -- It is possible that x considers y a friend, but not vice-versa.
    -- If every edge should be accompanied by its dual,
    -- use `undirectedGraphFromEdges`, below.
  } deriving (Show, Eq, Ord)
makeLenses ''Graph


-- | For each chain, construct all relevant chains that are one node longer.
-- If any of the new chains begins with `end`, return `count`.
-- Otherwise, delete every edge that was used, increment count, and repeat.
--
-- This is breadth-first search (BFS). I considered DFS also,
-- but BFS has better speed properties.
-- DFS could send you down needless rabbit holes,
-- searching for a long time when there's a much shorter path.
-- BFS is guaranteed to find the result in a number of iterations
-- equal to the length of the shortest chain, if one exists.
lengthOfShortestChain :: (Node,Node) -> Graph -> Either String Int
lengthOfShortestChain (start,end) g0 =
  if start == end
  then if elem start $ _gNodes g0
       then Right 0
       else Left $ show start ++ " is not part of the data."
  else go [[start]] 1 g0 where
  go :: [Chain] -> Int -> Graph -> Either String Int
  go cs count g = let
    cs' :: [Chain]
    cs' = concatMap (extendChain g) cs
    in if S.member end $ S.fromList $ map head cs'
       then Right count
    else if length cs' == 0
         then Left $ "No path leads from "
              ++ show start ++ " to " ++ show end ++ "."
         else let
    deleteOutEdges :: Node -> Graph -> Graph
    deleteOutEdges n = gEdges %~ M.delete n
    g' :: Graph
    g' = foldr deleteOutEdges g $ map head cs
    in go cs' (count+1) g'

extendChain :: Graph -> Chain -> [Chain]
extendChain _ [] = error "this should not happen"
extendChain g c@(h:_) = case M.lookup h $ _gEdges g of
  Nothing -> []
  Just friends -> map (:c) $ S.toList friends

-- | This assumes that every person in the graph has at least one friend.
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

undirectedGraphFromEdges :: [(Node,Node)] -> Graph
undirectedGraphFromEdges es =
  graphFromEdges $ es ++ map (\(x,y) -> (y,x)) es
