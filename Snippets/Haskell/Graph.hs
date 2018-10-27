module RFPathFinder where

data Edge a = Edge { edgeObject :: a
                   , edgeNodeA  :: Node a
                   , edgeNodeB  :: Node a
                   , edgeGraph  :: Graph a
                   }

data Node a = Node { nodeObject :: a
                   , nodeEdges  :: [Edge a]
                   , nodeGraph  :: Graph a
                   }

data Graph a = Graph { nodes :: [Node a]
                     , edges :: [Edge a]
                     }

main :: IO ()
main = putStrLn "hello"
