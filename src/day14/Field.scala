package day14

import general.Coordinates

case class Field(width:Int, height:Int) {
	private var robots:List[Robot] = Nil

	def addRobot(startRow:Int, startCol:Int, moveVertical:Int, moveHorizontal:Int):Unit = {
		val robot = new Robot
		robot.position = Coordinates(startRow, startCol)
		robot.moveOffset = Coordinates(moveVertical, moveHorizontal)
		robots ::= robot
	}

	def advanceAllRobots(seconds:Int):Unit = {
		robots.foreach(_.updatePosition(seconds))
	}

	def getQuadrantSecurityValues:Seq[Int] = {
		var ul = 0
		var ur = 0
		var ll = 0
		var lr = 0

		for (robot <- robots) {
			assert(robot.position.isWithinBoard(height, width))
			if (robot.position.row < height/2 && robot.position.col < width/2) {
				ul += 1
			} else if (robot.position.row < height/2 && robot.position.col > width/2) {
				ur += 1
			} else if (robot.position.row > height/2 && robot.position.col > width/2) {
				lr += 1
			} else if (robot.position.row > height / 2 && robot.position.col < width / 2) {
				ll += 1
			}

		}
		ul::ur::ll::lr::Nil
	}

	override def toString: String = {
		val builder = new StringBuilder()
		for (row <- 0 until height) {
			for (col <- 0 until width) {
				if (false && (row == height/2 || col == width/2)) {
					builder.append(" ")
				}
				else {
					robots.count(r => r.position == Coordinates(row, col)) match
						case 0 => builder.append(".")
						case x: Int => builder.append(x.toString)
				}
			}
			builder.append("\n")
		}
		builder.toString()
	}

	def countStacked:Int = {
		var stacked = 0

		for (row <- 0 until height) {
			for (col <- 0 until width) {
				if (robots.count(r => r.position == Coordinates(row, col)) > 1) {
					stacked += 1
				}
			}
		}

		stacked
	}

	class Robot {
		var position:Coordinates = _
		var moveOffset:Coordinates = _

		def updatePosition(seconds:Int):Unit = {
			position = position + (moveOffset * seconds)
			position = Coordinates(position.row % height, position.col % width) // can get negative
			if (position.row < 0) {
				position += Coordinates(height,0)
			}
			if (position.col < 0) {
				position += Coordinates(0,width)
			}
		}
	}
}
