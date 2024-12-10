package day10

import general.{Coordinates, DirectedGraph, InputReader, Vertex}

object Main10 {
	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day10-test")
		val input = InputReader.getLines("input/day10")

		val graph = buildGraph(input)

		var count = 0
		for (start <- graph.getAllVerticesWithName(0)) {
			count += graph.findAllPathsToName(start, 9).length
			//println(count)
		}

		println(count)
	}

	def buildGraph(input:List[String]):DirectedGraph[Int] = {
		val graph = new DirectedGraph[Int]()

		val arrayInput:Array[Array[Vertex[Int]]] = input.map(_.map(_.toString.toInt).map(graph.addVertex).toArray).toArray

		for (row <- arrayInput.indices) {
			for (col <- arrayInput(row).indices) {
				for (neighbour <- Coordinates(row,col).getManhattanNeighbours
					 if neighbour.isWithinBoard(arrayInput.length, arrayInput(row).length)) {
					if (arrayInput(row)(col).name + 1 == arrayInput(neighbour.row)(neighbour.col).name) {
						graph.addEdge(arrayInput(row)(col), arrayInput(neighbour.row)(neighbour.col), 1)
					}
				}
			}
		}

		graph
	}

}
