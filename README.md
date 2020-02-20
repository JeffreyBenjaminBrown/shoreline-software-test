# Running the code

The code is in the `src/` folder.

This code depends on the following Haskell packages:

* base
* containers
* HUnit
* lens (used on only two lines, one of which is the call to `makeLenses`)

The `stack.yaml` and `this.cabal` files are probably unnecessary,
but they describe the environment I used.

# Responses to the questions you posed.

The following provides an overview. The comments

## The data structure I chose

The `Graph` type includes a swet of nodes and a map from nodes to their out-neighbors, that is, the nodes that can be reached by following an edge in the forward direction. The advantages of this data structure are (1) testing whether a node is a member of the graph is O(1), and (2) finding the set of all out-neighbors of a node is as fast as possible, O(n), where n is the number of out-neighbors.

Had I been implementing an algorithm that required searching from a node to its in-neighbors, I would have included a third field, mapping nodes to their in-neighbors.

It probably would have been safe to use an undirected graph, since most people conceive of the friend relationship as symmetric. However, some don't, and directed graphs are more general, so I went with those.

## The algorithm I used

To find the shortest chain between two nodes, I used ordinary breadth-first search. It occurs to me now (a day later) that bidirectional search would be faster. If I had reason to believe it would pay off to bother rewriting the code, I would -- please let me know.

The only alternative I considered was depth-first search. That could send you down needless rabbit holes, searching for a long time when there's a much shorter path. BFS is guaranteed to find the result in a number of iterations equal to the length of the shortest chain, if one exists.

The algorithm depends on a function called `extendChain`. Given a graph `g` and a chain `c` the first member of which is `h`, `extendChain` returns all chains that can be constructed by prepending an out-neighbor of `h` to `c`. (The chain is stored in reverse order in the list because manipulating the head of a list is fast.)

The algorithm begins with a single chain that contains the start node. At each "iteration" (it's really a fold, there's no explicit loop) all chains are extended by one node. If any of them now ends in the desired target, the shortest has been found, and the number of iterations run is returned. Otherwise the process repeats.
