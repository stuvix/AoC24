package day6

trait Tile {
	def hasBeenVisited:Boolean = false
	def visit():Unit = {}

	/**
	 * @return true if this tile has already been visitied by that direction, false otherwise
	 */
	def visit(direction: Direction):Boolean = {false}
	def reset():Unit = {}
}

case class Obstacle() extends Tile {
	override def toString: String = "#"	
}

case class Ground() extends Tile {
	var visited = false
	var directions:List[Direction] = Nil

	override def hasBeenVisited: Boolean = visited
	
	override def visit(): Unit = visited = true
	
	override def toString: String = if hasBeenVisited then "X" else "."

	override def visit(direction: Direction): Boolean = {
		val beenThere = directions.contains(direction)
		directions ::= direction
		beenThere
	}

	override def reset(): Unit = {
		visited = false
		directions = Nil
	}
}

object Tile {
	def fromString(s:String):Tile = {
		s match
			case "#" => new Obstacle
			case "." => new Ground
			case "^" => new Ground
			case _ => throw new IllegalArgumentException()
	}
}
