package general

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

class Graph[T] {
  private val vertices = new mutable.HashMap[T, Vertex]()
  private var edges:List[Edge] = Nil
  private val shortestDistances = new mutable.HashMap[Vertex, Int]()

  def addVertex(name:T):Vertex = {
    val v = new Vertex(name)
    assert(!vertices.contains(name))
    vertices.addOne((name, v))
    v
  }

  def getVertex(name:T):Vertex = {
    vertices.getOrElse(name, throw new IllegalArgumentException())
  }

  def containsVertex(name: T): Boolean = {
    vertices.contains(name)
  }

  def addEdge(v1:T, v2:T, weight:Int):Unit = {
    this.addEdge(this.getVertex(v1), this.getVertex(v2), weight)
  }
  
  def addEdge(v1:Vertex, v2:Vertex, weight: Int):Unit = {
    val e = new Edge(v1,v2,weight)
    edges ::= e
    v1.addEdge(e)
    v2.addEdge(e)
  }

  def shortestDistanceForAllVertices(start:Vertex):Unit = {
    val priorityQueue = new mutable.PriorityQueue[(Vertex, Int)]()(Ordering.by(-_._2))

    priorityQueue.enqueue((start, 0))
    while (priorityQueue.nonEmpty) {
      val (node, distance) = priorityQueue.dequeue()

      if (shortestDistances.contains(node)) {
        // skip i guess
      }
      else {
        shortestDistances.put(node, distance)
        for (x <- node.getNeighbours.map(tuple => (tuple._1, distance + tuple._2))) {
          priorityQueue.enqueue(x)
        }
      }
    }
  }
  
  def isReachable(start:T, dest:T):Boolean = isReachable(getVertex(start), getVertex(dest))
  
  def isReachable(start:Vertex, dest:Vertex):Boolean = {
    val reached = new mutable.HashSet[Vertex]()
    var todo = start::Nil
    while (todo.nonEmpty) {
      val current = todo.head
      todo = todo.tail
      
      reached.add(current)
      for ((n,_) <- current.getNeighbours) {
        if (!reached.contains(n)) {
          todo ::= n
        }
      }
    }
    reached.contains(dest)
  }
  
  def removeVertex(name:T):Unit = {
    val v = vertices.remove(name).get
    for ((neighbour,_) <- v.getNeighbours) {
      neighbour.removeEdge(v)
    }    
    edges = edges.filterNot(_.containsVertex(v))
  }

  def getShortestDistanceForVertex(vertex:Vertex):Int = {
    shortestDistances(vertex)
  }

  def getAllShortestPathsVertices(from:Vertex, to:Vertex):Set[Vertex] = {
    val set = new mutable.HashSet[Vertex]()

    var todo = to :: Nil

    while (todo.nonEmpty) {
      val current = todo.head
      todo = todo.tail

      if (current == from || set.contains(current)) {
        // do nothing?
      }
      else {
        var minimum = Int.MaxValue
        for ((neighbour, weight) <- current.getNeighbours) {
          if (shortestDistances(neighbour) + weight < minimum) {
            minimum = shortestDistances(neighbour) + weight
          }
        }

        for ((neighbour, weight) <- current.getNeighbours) {
          if (shortestDistances(neighbour) + weight == minimum) {
            todo ::= neighbour
          }
        }
      }

      set.add(current)
    }

    set.toSet
  }

  def getAll3Cliques:List[List[Vertex]] = {
    val completedVertices = new mutable.HashSet[Vertex]()
    var cliques:List[List[Vertex]] = Nil
    for (vertex <- vertices.values) {
      for ((n1, n2) <- ListOps.generateUniquePairs(vertex.getNeighbours.map(_._1).toList)
           if !completedVertices.contains(n1) && !completedVertices.contains(n2)) {
        if (n1.getNeighbours.map(_._1).contains(n2)) {
          cliques ::= (vertex :: n1 :: n2 :: Nil)
        }
      }
      completedVertices.add(vertex)
    }
    cliques
  }

  def getLargestClique:List[Vertex] = {
    getAllMaximalCliques.maxBy(_.length)
  }

  private def getAllMaximalCliques:List[List[Vertex]] = {
    recursiveSubGraphCliqueFinding(vertices.values.toList).map(_.toList)
  }

  private def recursiveSubGraphCliqueFinding(vertices:List[Vertex]):List[Set[Vertex]] = {
    if (vertices.length == 1) {
      (vertices.head :: Nil).toSet :: Nil
    }
    else {
      val v = vertices.head
      val others = vertices.tail
      val rec = recursiveSubGraphCliqueFinding(others)

      val newOnes:mutable.HashSet[Set[Vertex]] = new mutable.HashSet[Set[Vertex]]()

      for (clique <- rec) {
        val attempt = clique.filter(_.bordersAll(v::Nil))
        if (attempt.nonEmpty) {
          if !newOnes.contains((v :: attempt.toList).toSet) then newOnes.add((v :: attempt.toList).toSet)
        }
      }
      if newOnes.isEmpty then newOnes.add((v :: Nil).toSet)

      println(vertices.length)

      rec ++ newOnes.toList
    }
  }

  def getVertexCount:Int = vertices.values.size

  class Vertex(name:T) {
    var edges:List[Edge] = Nil

    def addEdge(edge: Edge):Unit = {
      assert(edge.v1 == this || edge.v2 == this)
      edges ::= edge
    }

    def getNeighbours:Seq[(Vertex, Int)] = {
      for (edge <- edges)
        yield (if edge.v1 == this then edge.v2 else edge.v1, edge.weight)
    }

    override def toString: String = {
      "(" + name.toString + (if shortestDistances.contains(this) then f", ${shortestDistances(this)}" else "") + ")"
    }

    def getName:T = name
    
    def removeEdge(to:Vertex):Unit = {
      edges = edges.filterNot(_.containsVertex(to))
    }

    def degree:Int = edges.length

    def bordersAll(others:Iterable[Vertex]):Boolean = {
      boundary {
        val neighbours = getNeighbours.map(_._1).toSet
        for (other <- others) {
          if !neighbours.contains(other) then break(false)
        }
        true
      }
    }
  }

  class Edge(var v1:Vertex, var v2:Vertex, var weight:Int) {
    def getVertices:Seq[Vertex] = v1::v2::Nil
    
    def containsVertex(v:Vertex):Boolean = v1 == v || v2 == v 
  }
}
