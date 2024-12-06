package day6

trait Direction {
	def turnRight():Direction
	def applyMove(pos:(Int, Int)):(Int,Int)
}

case object Up extends Direction {
	override def turnRight(): Direction = Right
	def applyMove(pos:(Int, Int)):(Int,Int) = (pos._1 - 1, pos._2)
}

case object Right extends Direction {
	override def turnRight(): Direction = Down
	def applyMove(pos:(Int, Int)):(Int,Int) = (pos._1, pos._2 + 1)
}

case object Down extends Direction {
	override def turnRight(): Direction = Left
	def applyMove(pos:(Int, Int)):(Int,Int) = (pos._1 + 1, pos._2)
}

case object Left extends Direction {
	override def turnRight(): Direction = Up
	def applyMove(pos:(Int, Int)):(Int,Int) = (pos._1, pos._2 - 1)
}