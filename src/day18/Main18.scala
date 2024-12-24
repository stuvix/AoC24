package day18

import general.{Coordinates, Graph, InputReader}

object Main18 {
  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null
    var size:Int = 0
    var cutoff = 0

    if (testing) {
      input = InputReader.getLines("input/day18-test")
      size = 7
      cutoff = 12
    }
    else {
      input = InputReader.getLines("input/day18")
      size = 71
      cutoff = 1024
    }

    val memory = Array.ofDim[String](size, size)
    for (row <- memory.indices;
         col <- memory.head.indices) {
      memory(row)(col) = "."
    }

    for (i <- 0 until cutoff) {
      val coordinates = Coordinates.fromString(input(i)).reverse
      coordinates.arrayWrite(memory, "#")
    }

    //memory.foreach(line => {line.foreach(print);println()})

    val graph = toGraph(memory)
    graph.shortestDistanceForAllVertices(graph.getVertex(Coordinates(0,0)))
    println(graph.getShortestDistanceForVertex(graph.getVertex(Coordinates(size-1, size-1))))

    println("------------------")

    for (i <- cutoff until input.length) {
      val coordinates = Coordinates.fromString(input(i)).reverse
      if (graph.containsVertex(coordinates)) {
        graph.removeVertex(coordinates)
        if (!graph.isReachable(Coordinates(0,0), Coordinates(size-1, size-1))) {
          println(coordinates.reverse)
          sys.exit()
        }
      }
    }
  }



  private def toGraph(array:Array[Array[String]]):Graph[Coordinates] = {
    val graph = new Graph[Coordinates]

    for (row <- array.indices;
         col <- array.head.indices){
      val current = Coordinates(row, col)
      if (current.arrayAccess(array) == ".") {
        graph.addVertex(current)

        val left = current + general.Left.getOffset
        if (left.isWithinBoard(array.length, array.head.length) && left.arrayAccess(array) == ".") {
          graph.addEdge(current, left, 1)
        }

        val up = current + general.Up.getOffset
        if (up.isWithinBoard(array.length, array.head.length) && up.arrayAccess(array) == ".") {
          graph.addEdge(current, up, 1)
        }
      }
    }

    graph
  }
}
