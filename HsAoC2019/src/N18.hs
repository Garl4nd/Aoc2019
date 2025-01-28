module N18 where

import Useful 
import GraphUtils 

-- runDijkstraST :: forall graph node. (Show node, LabeledGraph graph node) => graph -> node -> [node] -> DijkstraState node
type Key = Char 
type AugPos = (GridPos, [Key])
