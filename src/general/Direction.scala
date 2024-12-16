package general

trait Direction {
	def getOffset:Coordinates
}

case object Up extends Direction {
	override def getOffset: Coordinates = Coordinates(-1,0)
}

case object Right extends Direction {
	override def getOffset: Coordinates = Coordinates(0,1)
}

case object Down extends Direction {
	override def getOffset: Coordinates = Coordinates(1,0)
}

case object Left extends Direction {
	override def getOffset: Coordinates = Coordinates(0,-1)
}

object Direction {
	def getAllDirections:List[Direction] =
		Up :: Right :: Down :: Left :: Nil
		
		
	def fromString(s: String): Direction = {
		s match
			case "v" => Down
			case "<" => Left
			case "^" => Up
			case ">" => Right
	}
}