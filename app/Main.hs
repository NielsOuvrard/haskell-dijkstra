module Main (main) where

import System.Environment
import System.Exit

import Debug.Trace (trace)
import Debug.Trace (traceShow)

-- * TYPE SYNONYMS
type Label = String
type Distance = Int
type Path = [Int]

type MyNode = (Label, Distance, Path)
type MyEdge = (Label, Label, Int)
type MyGraph = ([MyNode], [MyEdge])
type MyPath = [MyNode]

-- * FUNCTIONS

lookForNode :: [MyNode] -> Label -> MyNode
lookForNode [] _ = ("", 0, [])
lookForNode ((label, dist, path):xs) name
    | label == name = (label, dist, path)
    | otherwise = lookForNode xs name

getDistance :: MyNode -> Distance
getDistance (_, dist, _) = dist


getLabel :: MyNode -> Label
getLabel (label, _, _) = label

getSource :: MyEdge -> Label
getSource (src, _, _) = src

getDest :: MyEdge -> Label
getDest (_, dest, _) = dest

getWeight :: MyEdge -> Int
getWeight (_, _, weight) = weight

-- -- readGraph :: String -> IO Graph
-- -- readGraph str = ()


-- * original implementation
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
-- updateNodeListNext (x:[]) _ = x
updateNodeListNext ((label, distance, path):xs) node
    | label == getLabel node = (label, distance, path)
    | otherwise = updateNodeListNext xs node

--                 old     new_ones        updated
updateNodeList :: [MyNode] -> [MyNode] -> [MyNode]
updateNodeList [] _ = []
updateNodeList (node:xs) nodes = updateNodeListNext nodes node : updateNodeList xs nodes


initAllNodesStartToZero :: [MyNode] -> String -> [MyNode] -- put distance to infinity
initAllNodesStartToZero [] _ = []
initAllNodesStartToZero ((label, distance, path):xs) start 
    | label == start = [(label, 0, path)] ++ initAllNodesStartToZero xs start
    | otherwise = (label, 99, path) : initAllNodesStartToZero xs start

-- TODO see for the path
-- iterateThroughNeighbors :: MyGraph -> [MyNode] -> MyNode -> [MyNode]
-- iterateThroughNeighbors _ [] _ = []
-- iterateThroughNeighbors (nodes, edges) (n:neighbors) current_node
--     | getDistance current_node + getDistance n < getDistance (lookForNode nodes (getLabel n)) =
--         (getLabel n, getDistance current_node + getDistance n, []) : iterateThroughNeighbors (nodes, edges) neighbors current_node
--     | otherwise = iterateThroughNeighbors (nodes, edges) neighbors current_node

-- return nodes to update
iterateThroughNeighbors :: MyGraph -> [MyNode] -> MyNode -> [MyNode]
iterateThroughNeighbors _ [] _ = []
iterateThroughNeighbors (nodes, edges) (n:neighbors) current_node =
    let currentDistance = getDistance current_node
        neighborDistance = getDistance n
        existingNodeDistance = getDistance (lookForNode nodes (getLabel n))
        newDistance = currentDistance + neighborDistance
    in traceShow ("iterateThroughNeighbors Current node: " ++ show current_node ++ ", Neighbor: " ++ show n ++ ", Current Distance: " ++ show currentDistance ++ ", Neighbor Distance: " ++ show neighborDistance ++ ", Existing Node Distance: " ++ show existingNodeDistance ++ ", New Distance: " ++ show newDistance) $
       if newDistance < existingNodeDistance
       then (getLabel n, newDistance, []) : iterateThroughNeighbors (nodes, edges) neighbors current_node
       else iterateThroughNeighbors (nodes, edges) neighbors current_node


iterateThroughUnvisited :: MyGraph -> [Label] -> MyGraph
iterateThroughUnvisited (nodes, edges) [] = (nodes, edges)
iterateThroughUnvisited (nodes, edges) unvisited =
    let current_node = traceShow ("iterateThroughUnvisited Current node: " ++ show (nodeWithSmallestDistance (getAllThesesNodes nodes unvisited))) (nodeWithSmallestDistance (getAllThesesNodes nodes unvisited))
        neighbors = traceShow ("Neighbors of " ++ show current_node ++ ": " ++ show (getNeighborsOfNode edges (getLabel current_node))) (getNeighborsOfNode edges (getLabel current_node))
        updatedValues = (iterateThroughNeighbors (nodes, edges) neighbors current_node)
        updatedNodes = traceShow ("Updated nodes: ---" ++ show nodes ++ "---" ++ show updatedValues ++ " === " ++ show (updateNodeList nodes updatedValues)) (updateNodeList nodes updatedValues)
        remainingUnvisited = traceShow ("Remaining unvisited: " ++ show (removeThisNode unvisited current_node)) (removeThisNode unvisited current_node)
    -- in iterateThroughUnvisited (updatedNodes, edges) remainingUnvisited
   in current_node `seq` neighbors `seq` updatedValues `seq` updatedNodes `seq` remainingUnvisited `seq` iterateThroughUnvisited (updatedNodes, edges) remainingUnvisited

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

    let updatedGraph = iterateThroughUnvisited (initializedNodes, edges) unvisitedNodes
    trace ("Updated Graph: " ++ show updatedGraph) $ return ()

main :: IO ()
main = do
    args <- getArgs
    argManager args 0

-- A [("A",0,[]),("B",2,[]),("C",3,[]),("D",6,[]),("E",4,[]),("F",6,[])]
-- B ([("A",2,[]),("B",0,[]),("C",1,[]),("D",4,[]),("E",2,[]),("F",4,[])]
