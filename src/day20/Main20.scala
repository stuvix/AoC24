package day20

import general.{Coordinates, Graph, InputReader}

object Main20 {
  private var start:Coordinates = _
  private var end:Coordinates = _

  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null

    if (testing) {
      input = InputReader.getLines("input/day20-test")

    }
    else {
      input = InputReader.getLines("input/day20")

    }

    /*Coordinates(0,0).getManhattanNeighbours(4).foreach(println)
    Coordinates(0,0).getManhattanNeighbours(4).foreach(c  => assert(Coordinates(0,0).manhattanDistanceTo(c) <= 4))
    println(Coordinates(0,0).getManhattanNeighbours(4).length)
    sys.exit()*/

    val array = input.map(_.toCharArray.map(_.toString)).toArray

    val (start, end, graph) = toGraph(array)
    Main20.start = start
    Main20.end = end
    graph.shortestDistanceForAllVertices(graph.getVertex(start))
    val baseline = graph.getShortestDistanceForVertex(graph.getVertex(end))
    println(baseline)

    var shortcuts:List[Int] = Nil

    val distances = Array.ofDim[Int](array.length, array.head.length)
    for (row <- distances.indices;
         col <- distances.head.indices) {
      val coordinates= Coordinates(row, col)
      if (coordinates.arrayAccess(array) == ".") {
        coordinates.arrayWrite(distances, graph.getShortestDistanceForVertex(graph.getVertex(coordinates)))
      }
      else {
        coordinates.arrayWrite(distances, -1)
      }
    }

    for (row <- distances.indices) {
      for (col <- distances.head.indices) {
        val current = Coordinates(row, col)
        val currentDistance = current.arrayAccessOr(distances, -2)
        if (currentDistance > -1) {
          for (neighbour:Coordinates <- current.getManhattanNeighbours(20)
               if neighbour.arrayContainsCoordinates(distances)) {
            if (neighbour.arrayAccess(distances) > currentDistance) {
              shortcuts ::= neighbour.arrayAccess(distances) - currentDistance - current.manhattanDistanceTo(neighbour)
            }
          }
        }
      }
    }

    shortcuts.groupBy(i => i).toList.sortBy(_._1).foreach(t => println(f"${t._1} - ${t._2.length}"))
    println(shortcuts.count(_ >= 100))
  }

  private def test(array:Array[Array[Int]], coordinates: Coordinates*):Int = {
    assert(coordinates.length == 2)
    Math.abs(coordinates.head.arrayAccess(array) - coordinates.tail.head.arrayAccess(array)) - coordinates.head.manhattanDistanceTo(coordinates.tail.head)
  }

  // find every instance of ".##." and ".#.", either horizontally or vertically. The calculate the distance for each.

  private def toGraph(array:Array[Array[String]]):(Coordinates, Coordinates, Graph[Coordinates]) = {
    val graph = new Graph[Coordinates]
    var start:Coordinates = null
    var end: Coordinates = null

    for (row <- array.indices;
         col <- array.head.indices) {
      val current = Coordinates(row, col)
      if (current.arrayAccess(array).matches("[.SE]")) {
        graph.addVertex(current)

        if (current.arrayAccess(array) == "S") {
          start = current
          current.arrayWrite(array, ".")
        }

        if(current.arrayAccess(array) == "E") {
          end = current
          current.arrayWrite(array, ".")
        }

        val left = current + general.Left.getOffset
        if (left.isWithinBoard(array.length, array.head.length) && left.arrayAccess(array).matches("[.SE]")) {
          graph.addEdge(current, left, 1)
        }

        val up = current + general.Up.getOffset
        if (up.isWithinBoard(array.length, array.head.length) && up.arrayAccess(array).matches("[.SE]")) {
          graph.addEdge(current, up, 1)
        }
      }
    }

    (start, end, graph)
  }
}
