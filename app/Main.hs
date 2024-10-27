module Main (main) where

import System.Environment
import System.Exit

import Lib (readGraphFromFile)

-- * TYPE SYNONYMS
type Label = String
type Distance = Int
type Path = [Label]

type MyNode = (Label, Distance, Path)
type MyEdge = (Label, Label, Int)
type MyGraph = ([MyNode], [MyEdge])

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
nodeWithSmallestDistance [] = ("", 0, [])
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
initAllNodesStartToZero ((label, _, path):xs) start 
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

prettyPrintPath :: [String] -> String
prettyPrintPath [] = ""
prettyPrintPath (x:[]) = x
prettyPrintPath (x:xs) = x ++ " -> " ++ prettyPrintPath xs

prettyPrintData :: String -> String -> [String] -> Int -> String
prettyPrintData start end path dist =
    "Shortest path from " ++ start ++ " to " ++ end ++ " is:\n" ++ prettyPrintPath path ++ "\nWith a distance of " ++ show dist

-- INFO: Main function
argManager :: [String] -> Int -> IO ()
argManager ("-h" : remainingArgs) n = do
    putStrLn "Usage: ./haskell-dijkstra [-h] <graph_path> <node_start> <node_end>"
    putStrLn "\nArguments:"
    putStrLn "\t-h"
    putStrLn "\t\tShow this help message and exit"
    putStrLn "\t<graph_path>"
    putStrLn "\t\tPath to the graph file"
    putStrLn "\t<node_start>"
    putStrLn "\t\tStart node"
    putStrLn "\t<node_end>"
    putStrLn "\t\tEnd node"
    argManager remainingArgs (n + 1)
argManager (graphPath : nameStart : nameEnd : _) _ = do
    (nodes, edges) <- readGraphFromFile graphPath

    let initializedNodes = initAllNodesStartToZero nodes nameStart
    let unvisitedNodes = getAllNodesLabels nodes

    let (updatedNodes, _) = iterateThroughUnvisited (initializedNodes, edges) unvisitedNodes
    let (_, dist, path) = lookForNode updatedNodes nameEnd
    putStrLn $ prettyPrintData nameStart nameEnd (path ++ [nameEnd]) dist
argManager _ _ = do
    putStrLn "Invalid arguments. Use -h for help."

main :: IO ()
main = do
    args <- getArgs
    argManager args 0
    System.Exit.exitSuccess