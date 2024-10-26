module Main (main) where

import System.Environment
import System.Exit

import Debug.Trace (trace)
import Debug.Trace (traceShow)

-- * TYPE SYNONYMS
type Label = String
type Distance = Int
type Path = [Label]

type MyNode = (Label, Distance, Path)
type MyEdge = (Label, Label, Int)
type MyGraph = ([MyNode], [MyEdge])
type MyPath = [MyNode]

-- * FUNCTIONS

getDistance :: MyNode -> Distance
getDistance (_, dist, _) = dist

getPath :: MyNode -> Path
getPath (_, _, path) = path

getLabel :: MyNode -> Label
getLabel (label, _, _) = label


getSource :: MyEdge -> Label
getSource (src, _, _) = src

getDest :: MyEdge -> Label
getDest (_, dest, _) = dest

getWeight :: MyEdge -> Int
getWeight (_, _, weight) = weight


lookForNode :: [MyNode] -> Label -> MyNode
lookForNode [] _ = ("", 0, [])
lookForNode ((label, dist, path):xs) name
    | label == name = (label, dist, path)
    | otherwise = lookForNode xs name

-- -- readGraph :: String -> IO Graph
-- -- readGraph str = ()


nodeWithSmallestDistance :: [MyNode] -> MyNode
nodeWithSmallestDistance [node] = node
nodeWithSmallestDistance (n1:n2:ns)
    | getDistance n1 <= getDistance n2 = nodeWithSmallestDistance (n1:ns)
    | otherwise = nodeWithSmallestDistance (n2:ns)

removeThisNode :: [Label] -> MyNode -> [Label]
removeThisNode [] _ = []
removeThisNode (x:xs) node
    | x == getLabel node = xs
    | otherwise = x : removeThisNode xs node

getNeighborsOfNode :: [MyEdge] -> Label -> [MyNode]
getNeighborsOfNode [] _ = []
getNeighborsOfNode (x:xs) label
    | getSource x == label = (getDest x, getWeight x, []) : getNeighborsOfNode xs label
    | otherwise = getNeighborsOfNode xs label

updateNodeListNext :: [MyNode] -> MyNode -> MyNode
updateNodeListNext [] node = node
updateNodeListNext ((label, distance, path):xs) node
    | label == getLabel node = (label, distance, path)
    | otherwise = updateNodeListNext xs node

--                 old     new_ones        updated
updateNodeList :: [MyNode] -> [MyNode] -> [MyNode]
updateNodeList [] _ = []
updateNodeList (node:xs) nodes = updateNodeListNext nodes node : updateNodeList xs nodes


initAllNodesStartToZero :: [MyNode] -> String -> [MyNode]
initAllNodesStartToZero [] _ = []
initAllNodesStartToZero ((label, distance, path):xs) start 
    | label == start = [(label, 0, path)] ++ initAllNodesStartToZero xs start
    | otherwise = (label, maxBound, path) : initAllNodesStartToZero xs start

iterateThroughNeighbors :: MyGraph -> [MyNode] -> MyNode -> [MyNode]
iterateThroughNeighbors _ [] _ = []
iterateThroughNeighbors (nodes, edges) (n:neighbors) current_node
    | getDistance current_node + getDistance n < getDistance (lookForNode nodes (getLabel n)) =
        (getLabel n, getDistance current_node + getDistance n, (getPath current_node) ++ [getLabel current_node]) : iterateThroughNeighbors (nodes, edges) neighbors current_node
    | otherwise = iterateThroughNeighbors (nodes, edges) neighbors current_node

iterateThroughUnvisited :: MyGraph -> [Label] -> MyGraph
iterateThroughUnvisited (nodes, edges) [] = (nodes, edges)
iterateThroughUnvisited (nodes, edges) unvisited =
    let current_node = (nodeWithSmallestDistance (getAllThesesNodes nodes unvisited))
        neighbors = (getNeighborsOfNode edges (getLabel current_node))
        updatedValues = (iterateThroughNeighbors (nodes, edges) neighbors current_node)
        updatedNodes = (updateNodeList nodes updatedValues)
        remainingUnvisited = (removeThisNode unvisited current_node)
    in iterateThroughUnvisited (updatedNodes, edges) remainingUnvisited

getAllThesesNodes :: [MyNode] -> [Label] -> [MyNode]
getAllThesesNodes [] _ = []
getAllThesesNodes nodes labels = filter (\(label, _, _) -> label `elem` labels) nodes

getAllNodesLabels :: [MyNode] -> [Label]
getAllNodesLabels [] = []
getAllNodesLabels (x:xs) = getLabel x : getAllNodesLabels xs

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
argManager (graphPath : nameStart : nameEnd : _) _ = do

    let nodes :: [MyNode]
        nodes = [("A", 0, []), ("B", 0, []), ("C", 0, []), ("D", 0, []), ("E", 0, []), ("F", 0, [])]

    let edges :: [MyEdge]
        edges = [ ("A", "B", 2), ("B", "A", 2)
                , ("B", "C", 1), ("C", "B", 1)
                , ("C", "E", 3), ("E", "C", 3)
                , ("E", "B", 2), ("B", "E", 2)
                , ("A", "C", 4), ("C", "A", 4)
                , ("D", "B", 4), ("B", "D", 4)
                , ("D", "E", 3), ("E", "D", 3)
                , ("D", "F", 2), ("F", "D", 2)
                , ("F", "E", 2), ("E", "F", 2)
                ]
    
    let graph :: MyGraph
        graph = (nodes, edges)

    let initializedNodes = initAllNodesStartToZero nodes nameStart
    let unvisitedNodes = getAllNodesLabels nodes

    let (updatedNodes, updatedEdges) = iterateThroughUnvisited (initializedNodes, edges) unvisitedNodes
    let (name, dist, path) = lookForNode updatedNodes nameEnd
    print ("Shortest path from " ++ nameStart ++ " to " ++ nameEnd ++ " is: " ++ show path ++ " with a distance of " ++ show dist)

main :: IO ()
main = do
    args <- getArgs
    argManager args 0

