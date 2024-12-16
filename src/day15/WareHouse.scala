package day15

import general.{Coordinates, Direction}

class WareHouse {
	var robot: Coordinates = _
	var tiles: Array[Array[Tile]] = _

	private def executeMove(move: Direction): Unit = {
		if ((robot + move.getOffset).arrayAccess(tiles).canStepOnto(move, robot + move.getOffset)) {
			var moveTarget = robot + move.getOffset
			var movedObject = moveTarget.arrayAccess(tiles)
			moveTarget.arrayWrite(tiles, Robot)
			robot.arrayWrite(tiles, Empty)
			robot = moveTarget

			if (robot == Coordinates(7,5)) {
				//println()
			}

			if (movedObject == Box) {
				moveTarget += move.getOffset
				while (moveTarget.arrayAccess(tiles) == Box) {
					moveTarget += move.getOffset
				}
				assert(moveTarget.arrayAccess(tiles) == Empty)
				moveTarget.arrayWrite(tiles, Box)
			}
		}
	}

	def executeMoves(list:Seq[Direction]):Unit = {
		list.foreach(executeMove)
	}

	def getBoxCoordinates:Seq[Coordinates] = {
		for (row <- tiles.indices;
			 col <- tiles.head.indices
			 if tiles(row)(col) == Box)
			yield Coordinates(row,col)
	}

	override def toString: String = {
		val builder = new StringBuilder()
		for (row <- tiles.indices) {
			for (col <- tiles(row).indices) {
				builder.append(tiles(row)(col).toString)
			}
			builder.append("\n")
		}
		builder.toString()
	}

	trait Tile {
		def canStepOnto(move: Direction, target: Coordinates):Boolean
	}

	case object Wall extends Tile {
		override def canStepOnto(move: Direction, target: Coordinates): Boolean = false

		override def toString: String = "#"
	}

	case object Box extends Tile {
		override def canStepOnto(move: Direction, target: Coordinates): Boolean = {
			val whereTo = target + move.getOffset
			whereTo.arrayAccess(tiles).canStepOnto(move, whereTo)
		}

		override def toString: String = "O"
	}

	case object Empty extends Tile {
		override def canStepOnto(move: Direction, target: Coordinates): Boolean = true

		override def toString: String = "."
	}

	case object Robot extends Tile {
		override def canStepOnto(move: Direction, target: Coordinates): Boolean = true

		override def toString: String = "@"
	}

	object Tile {
		def fromString(s: String): Tile = {
			s match
				case "#" => Wall
				case "O" => Box
				case "." => Empty
				case "@" => Robot
				case _ => throw new IllegalArgumentException()
		}
	}
}

object WareHouse {
	def fromString(input:List[String]):WareHouse = {
		val house = new WareHouse

		house.tiles = input.map(s => s.map(c => c.toString).map(house.Tile.fromString).toArray).toArray

		for (row <- house.tiles.indices;
			 col <- house.tiles.head.indices) {
			if (house.tiles(row)(col) == house.Robot) {
				house.robot = Coordinates(row,col)
			}
		}

		house
	}
}

