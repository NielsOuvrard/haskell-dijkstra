# haskell-dijkstra

Implementation of Dijkstra's algorithm in Haskell.
No external libraries are used.

## Usage

```sh
./haskell-dijkstra [-h] <graph_path> <node_start> <node_end>
```

Arguments

- `-h`
  Show this help message and exit
- `<graph_path>`
  Path to the graph file
- `<node_start>`
  Start node
- `<node_end>`
  End node

Invalid arguments. Use -h for help.

## Example of usage

```sh
stack exec haskell-dijkstra-exe graph.txt F A
```

## Output

```
Shortest path from F to A is:
F -> E -> B -> A
With a distance of 6
```

## Graph file format

The graph file must be a text file with the following format:

```
node1 node2 distance
node2 node3 distance
...
```

For example:

```
A B 1
B C 2
C D 3
D A 4
```

## Build

```sh
stack build
```
