package day23

import general.{Graph, InputReader}

object Main23 {
  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null

    if (testing) {
      input = InputReader.getLines("input/day23-test")

    }
    else {
      input = InputReader.getLines("input/day23")

    }


    val graph = new Graph[String]
    for (line <- input) {
      val pcs = line.split("-")
      for (pc <- pcs) {
        if (!graph.containsVertex(pc) ) {
          graph.addVertex(pc)
        }
      }
      graph.addEdge(pcs.head, pcs.last, 1)
    }

    val cliques = graph.getAll3Cliques
    cliques.filter(clique => clique.exists(_.getName.startsWith("t"))).foreach(println)
    println(cliques.count(clique => clique.exists(_.getName.startsWith("t"))))

    println(graph.getVertexCount)

    println(graph.getLargestClique.map(_.getName).sorted.mkString(","))
  }

  def anyStartsWithT(list:Seq[String]):Boolean = {
    list.exists(_.startsWith("T"))
  }
}
