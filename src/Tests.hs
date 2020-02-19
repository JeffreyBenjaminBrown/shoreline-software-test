module Tests where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Either
import           Test.HUnit

import           Lib


tests :: IO Counts
tests = runTestTT $ TestList
  [ test_lengthOfShortestChain
  , test_extendChain
  , test_graphFromEdges
  , test_symmetricGraphFromEdges
  ]

-- | The data contains two connected components: one contains
-- 100 and 101, the other contains the rest.
-- It contains a long chain 0-10-20-30-40,
-- but 10 is connected to 30 also,
-- thus allowing me to test whether the algorithm in fact finds the shortcut.
testGraph :: Graph
testGraph = graphFromEdges
     [ (0,10),  (0,11),
       (10,20), (10,30),
       (20,30),
       (30,40),
       (100,101)]

-- | The interesting cases seemed to be:
-- nodes that do not exist,
-- nodes that are not connected,
-- the chain from something to itself,
-- nodes that are connected with a length-1 path, and
-- nodes that are connected by a long path and a shorter alternative.
-- I also test the first case in the definition, where start=end.
test_lengthOfShortestChain :: Test
test_lengthOfShortestChain = TestCase $ do
  assertBool "nodes that do not exist are not connected" $ isLeft $
    lengthOfShortestChain (9999,7777) testGraph
  assertBool "20 to 101: not connected" $ isLeft $
    lengthOfShortestChain (20,101) testGraph
  assertBool "0 to 0: connected by 0 hops" $
    lengthOfShortestChain (0,0) testGraph == Right 0
  assertBool "0 to 10: connected by 1 hop" $
    lengthOfShortestChain (0,10) testGraph == Right 1
  assertBool "0 to 40: connected by 3 hops (0-10-30-40)" $
    lengthOfShortestChain (0,40) testGraph == Right 3
  assertBool "nodes that do not exist are not connected" $ isLeft $
    lengthOfShortestChain (9999,7777) testGraph

-- | The interesting cases seemed to be:
-- A path that can be extended, and one that cannot.
test_extendChain :: Test
test_extendChain = TestCase $ do
  assertBool "0 is connected to 10 and 11" $
    S.fromList (extendChain testGraph [0]) ==
    S.fromList [[10,0],[11,0]]
  assertBool "11 has no outbound connections, so this path cannot be extended" $
    extendChain testGraph [11] == []
  assertBool "only the front of the list is relevant" $
    extendChain testGraph [11,123456789,error "meh"] == []

-- | This builds a directed graph.
-- The first test is the simplest possible graph.
-- There are lots of other cases I could test, but the interesting ones
-- seemed to be (1) making sure that something with multiple out-neigbors
-- is made correctly, and (2) the dual of (1).
test_graphFromEdges :: Test
test_graphFromEdges = TestCase $ do
  assertBool "2 nodes, 1 edge" $
    graphFromEdges [(1,2)] ==
    Graph { _gNodes = S.fromList [1,2]
          , _gOutEdges = M.fromList
            [ (1, S.singleton 2) ] }
  assertBool "an outward V shape" $
    graphFromEdges [(1,2), (1,3)] ==
    Graph { _gNodes = S.fromList [1,2,3]
          , _gOutEdges = M.fromList
            [ (1, S.fromList [2,3]) ] }
  assertBool "an inward V shape" $
    graphFromEdges [(2,1), (3,1)] ==
    Graph { _gNodes = S.fromList [1,2,3]
          , _gOutEdges = M.fromList
            [ (2, S.fromList [1])
            , (3, S.fromList [1]) ] }

-- | This builds a (still directed but) symmetric graph.
-- This function is almost too simple to test --
-- if it reverses one edge, it reverses them all,
-- hence a single test case seems sufficient.
test_symmetricGraphFromEdges :: Test
test_symmetricGraphFromEdges = TestCase $ do
  assertBool "2 nodes, 1 edge" $
    symmetricGraphFromEdges [(1,2)] ==
    Graph { _gNodes = S.fromList [1,2]
          , _gOutEdges = M.fromList
            [ (1, S.singleton 2)
            , (2, S.singleton 1) ] }
