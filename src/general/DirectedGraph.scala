package general

class DirectedGraph[T] {
	var vertices:List[Vertex[T]] = Nil
	var edges:List[Edge[T]] = Nil

	def addVertex(name:T):Vertex[T] = {
		val v = Vertex[T](name)
		vertices ::= v
		v
	}

	def addEdge(start:Vertex[T], end:Vertex[T], weight:Int):Unit = {
		val edge = Edge[T](start, end, weight)
		edges ::= edge
		start.addOutgoingEdge(edge)
		end.addIncomingEdge(edge)
	}

	def findAllPathsToName(start:Vertex[T], endName:T):List[List[Vertex[T]]] = {
		findAllPathsToNameRec(start, endName, Nil)
	}

	private def findAllPathsToNameRec(start: Vertex[T], endName: T, pathToStart:List[Vertex[T]]): List[List[Vertex[T]]] = {
		var paths: List[List[Vertex[T]]] = Nil

		for (edge <- start.outgoingEdges) {
			if (edge.end.name == endName) {
				paths ::= (edge.end :: pathToStart)
			}
			else {
				val newPaths = findAllPathsToName(edge.end, endName)
				for (path <- newPaths) {
					if (path.head.name == endName) {
						paths ::= path
					}
				}
			}
		}

		paths
	}
	
	def findReachableWithName(start:Vertex[T], name:T):List[Vertex[T]] = {
		unmarkAll()
		var todo = start :: Nil
		var found:List[Vertex[T]] = Nil
		while (todo.nonEmpty) {
			val current = todo.head
			todo = todo.tail
			if (current.name == name && !current.mark) {
				found ::= current
				current.mark = true
			}
			todo ++= current.outgoingEdges.map(_.end)
		}
		unmarkAll()
		found
	}
	
	def getAllVerticesWithName(name:T):Seq[Vertex[T]] = {
		vertices.filter(_.name == name)
	}
	
	private def unmarkAll():Unit = {
		vertices.foreach(_.mark = false)
	}
}

case class Vertex[T](name:T) {
	var outgoingEdges:List[Edge[T]] = Nil
	var incomingEdges:List[Edge[T]] = Nil
	
	var mark:Boolean = false

	def addIncomingEdge(edge: Edge[T]):Unit = {
		incomingEdges ::= edge
	}

	def addOutgoingEdge(edge: Edge[T]):Unit = {
		outgoingEdges ::= edge
	}
}

case class Edge[T](start:Vertex[T], end:Vertex[T], weight:Int) {

}
