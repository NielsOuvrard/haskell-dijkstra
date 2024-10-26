module Main (main) where


import System.Environment
import System.Exit

-- Import the necessary modules from the fgl library
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.NodeMap (insMapNodeM, insMapEdgeM)
import Data.Graph.Inductive.Query.DFS (dfs)

-- Define your graph type
type NodeLabel = String
type EdgeLabel = String
type MyGraph = Gr NodeLabel EdgeLabel


-- INFO: Main function
argManager :: [String] -> Int -> IO ()
argManager ("-h" : remainingArgs) n =
    putStrLn ("Usage: ./haskell-dijkstra [-h] <graph_path> <node_start> <node_end>") >>
    putStrLn ("\nArguments:") >>
    putStrLn ("\t-h") >>
    putStrLn ("\t\tShow this help message and exit") >>
    putStrLn ("\t<graph_path>") >>
    putStrLn ("\t\tPath to the graph file") >>
    putStrLn ("\t<node_start>") >>
    putStrLn ("\t\tStart node") >>
    putStrLn ("\t<node_end>") >>
    putStrLn ("\t\tEnd node") >>
    argManager remainingArgs (n + 1)
argManager (graphPath : nodeStart : nodeEnd : _) _ = do

    -- Define nodes with labels
    let nodes = [(1, "A"),(2, "B"),(3, "C")]
    -- Define edges with weights
    let edges = [(1, 2, 10),(2, 3, 5),(1, 3, 15)]
    -- Create a graph
    let graph = mkGraph nodes edges :: Gr String Int
    -- graph <- readGraph graphPath
    -- let path = dijkstra graph nodeStart nodeEnd

    -- putStrLn ("Shortest path from " ++ show start ++ " to " ++ show end ++ ": " ++ show path)
    putStrLn ("nodes = " ++ show nodes ++ "\nedges = " ++ show edges)
    putStrLn ("graph = " ++ show graph)

main :: IO ()
main = do
    args <- getArgs
    argManager args 0


-- readGraph :: String -> IO Graph
-- readGraph str = ()

-- dijkstra :: Graph -> Node -> Node -> Path
-- dijkstra graph node_start node_end = ()