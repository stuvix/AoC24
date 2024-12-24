package day16

import general.{Coordinates, Direction, Graph, InputReader}

object Main16 {
  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null

    if (testing) {
      input = InputReader.getLines("input/day16-test")

    }
    else {
      input = InputReader.getLines("input/day16")

    }

    val (graph, startingCoordinates, destinationCoordinates) = parseInput(input)

    graph.shortestDistanceForAllVertices(graph.getVertex((startingCoordinates, general.Right)))

    val dest = getVerticesWithCoordinates(destinationCoordinates, graph).map(v => (v,graph.getShortestDistanceForVertex(v))).minBy(_._2)

    println(dest._1)

    val set = graph.getAllShortestPathsVertices(graph.getVertex((startingCoordinates, general.Right)), dest._1)

    println(set.map(v => v.getName._1).toSet.size)
  }

  private def parseInput(list:List[String]):(Graph[(Coordinates, Direction)], Coordinates, Coordinates) = {
    val g = new Graph[(Coordinates, Direction)]
    var start:Coordinates = null
    var end: Coordinates = null

    val array = list.map(string => string.toCharArray.map(_.toString)).toArray
    for (row <- array.indices) {
      for (col <- array.head.indices) {
        val coordinates = Coordinates(row, col)
        if (coordinates.arrayAccess(array) == "." || coordinates.arrayAccess(array) == "S" || coordinates.arrayAccess(array) == "E") {
          val left = g.addVertex((coordinates, general.Left))
          val right = g.addVertex((coordinates, general.Right))
          val up = g.addVertex((coordinates, general.Up))
          val down = g.addVertex((coordinates, general.Down))
          g.addEdge(left, right, 1)
          g.addEdge(up, down, 1)
          g.addEdge(up, right, 1001)
          g.addEdge(right, down, 1001)
          g.addEdge(down, left, 1001)
          g.addEdge(left, up, 1001)
          if (coordinates.arrayAccess(array) == "S")
            start = coordinates
          if  (coordinates.arrayAccess(array) == "E")
            end = coordinates
        }
      }
    }
    for (row <- array.indices) {
      for (col <- array.head.indices) {
        val coordinates = Coordinates(row, col)
        if (coordinates.arrayAccess(array) == "." || coordinates.arrayAccess(array) == "S" || coordinates.arrayAccess(array) == "E") {
          val right = coordinates + general.Right.getOffset
          if (right.isWithinBoard(array.length, array.head.length) &&
            right.arrayAccess(array).matches("""[.SE]""")) {
            g.addEdge(g.getVertex((coordinates, general.Right)), g.getVertex((right, general.Left)), 0)
          }
          val down = coordinates + general.Down.getOffset
          if (down.isWithinBoard(array.length, array.head.length) &&
            down.arrayAccess(array).matches("""[.SE]""")) {
            g.addEdge(g.getVertex((coordinates, general.Down)), g.getVertex((down, general.Up)), 0)
          }
        }
      }
    }

    (g, start, end)
  }

  private def getVerticesWithCoordinates(coordinates: Coordinates, graph:Graph[(Coordinates, Direction)]):Seq[graph.Vertex] = {
    for (direction <- Direction.getAllDirections)
      yield graph.getVertex((coordinates, direction))
  }
}
