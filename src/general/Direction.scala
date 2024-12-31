package general

trait Direction {
	def getOffset:Coordinates
	
	def asString:String
}

case object Up extends Direction {
	override def getOffset: Coordinates = Coordinates(-1,0)

	override def asString: String = "^"
}

case object Right extends Direction {
	override def getOffset: Coordinates = Coordinates(0,1)

	override def asString: String = ">"
}

case object Down extends Direction {
	override def getOffset: Coordinates = Coordinates(1,0)

	override def asString: String = "v"
}

case object Left extends Direction {
	override def getOffset: Coordinates = Coordinates(0,-1)

	override def asString: String = "<"
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