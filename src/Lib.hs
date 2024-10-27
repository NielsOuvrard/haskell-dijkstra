module Lib
    ( readGraphFromFile
    ) where

import Data.List (nub)
import Text.Read (readMaybe)

type Label = String
type Distance = Int
type Path = [Label]
type MyNode = (Label, Distance, Path)
type MyEdge = (Label, Label, Int)
type MyGraph = ([MyNode], [MyEdge])

-- Parse a single line of the form "A B 4"
parseLine :: String -> Maybe MyEdge
parseLine line = case words line of
    [node1, node2, weightStr] -> do
        weight <- readMaybe weightStr :: Maybe Int
        return (node1, node2, weight)
    _ -> Nothing

-- Create nodes with initial distances and empty paths
createNodes :: [MyEdge] -> [MyNode]
createNodes edges = 
    let labels = nub $ concat [[n1, n2] | (n1, n2, _) <- edges]
    in [(label, maxBound, []) | label <- labels]

-- Parse the file content into a MyGraph
parseGraph :: String -> MyGraph
parseGraph content =
    let edges = mapMaybe parseLine (lines content)
        nodes = createNodes edges
    in (nodes, edges)

-- Main function to read the graph from a file
readGraphFromFile :: FilePath -> IO MyGraph
readGraphFromFile filePath = do
    content <- readFile filePath
    return (parseGraph content)

-- Utility function to safely parse lines
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of
                                Just y -> y : acc
                                Nothing -> acc) []
