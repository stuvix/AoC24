package day15

import general.{Coordinates, Direction, Down, Up}

class WareHouse {
	var robot: Coordinates = _
	var tiles: Array[Array[Tile]] = _

	private def executeMove(move: Direction): Unit = {
		if ((robot + move.getOffset).arrayAccess(tiles) == Empty) {
			val moveTarget = robot + move.getOffset
			moveTarget.arrayWrite(tiles, Robot)
			robot.arrayWrite(tiles, Empty)
			robot = moveTarget
		}
		else if ((robot + move.getOffset).arrayAccess(tiles) == BoxLeft ||
			(robot + move.getOffset).arrayAccess(tiles) == BoxRight) {
			if ((robot + move.getOffset).arrayAccess(tiles).canStepOnto(move, robot + move.getOffset)) {
				pushBox(robot + move.getOffset, move)
				if ((robot + move.getOffset).arrayAccess(tiles) == Empty) {
					val moveTarget = robot + move.getOffset
					moveTarget.arrayWrite(tiles, Robot)
					robot.arrayWrite(tiles, Empty)
					robot = moveTarget
				}
			}
		}
	}

	private def pushBox(anySide:Coordinates, dir:Direction): Boolean = {
		// get back to assuming the move is possible
		if (anySide.arrayAccess(tiles) == BoxLeft || anySide.arrayAccess(tiles) == BoxRight) {
			val otherSide = if anySide.arrayAccess(tiles) == BoxLeft then anySide + Coordinates(0, 1) else anySide + Coordinates(0, -1)
			val boxLeft = if anySide.arrayAccess(tiles) == BoxLeft then anySide else otherSide
			val boxRight = if anySide.arrayAccess(tiles) == BoxRight then anySide else otherSide
			var canMove = true

			dir match
				case Down | Up =>
					canMove &= pushBox(boxLeft + dir.getOffset, dir)
					canMove &= pushBox(boxRight + dir.getOffset, dir)
				case general.Left =>
					canMove &= pushBox(boxLeft + dir.getOffset, dir)
				case general.Right =>
					canMove &= pushBox(boxRight + dir.getOffset, dir)

			if (canMove) {
				boxLeft.arrayWrite(tiles, Empty)
				boxRight.arrayWrite(tiles, Empty)
				(boxLeft + dir.getOffset).arrayWrite(tiles, BoxLeft)
				(boxRight + dir.getOffset).arrayWrite(tiles, BoxRight)
				true
			}
			else {
				false
			}
		}
		else if (anySide.arrayAccess(tiles) == Wall) {
			false
		}
		else {
			assert(anySide.arrayAccess(tiles) == Empty)
			true
		}
	}

	def executeMoves(list:Seq[Direction]):Unit = {
		list.foreach(executeMove)
	}

	def getBoxCoordinates:Seq[Coordinates] = {
		for (row <- tiles.indices;
			 col <- tiles.head.indices
			 if tiles(row)(col) == BoxLeft)
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

	case object BoxLeft extends Tile {
		override def canStepOnto(move: Direction, target: Coordinates): Boolean = {
			val whereTo = target + move.getOffset
			val boxRightTarget = whereTo + Coordinates(0,1)
			move match
				case Down | Up => whereTo.arrayAccess(tiles).canStepOnto(move, whereTo) &&
					boxRightTarget.arrayAccess(tiles).canStepOnto(move,boxRightTarget)
				case general.Left => whereTo.arrayAccess(tiles).canStepOnto(move, whereTo)
				case general.Right => whereTo.arrayAccess(tiles).canStepOnto(move, whereTo)
		}

		override def toString: String = "["
	}

	case object BoxRight extends Tile {
		override def canStepOnto(move: Direction, target: Coordinates): Boolean = {
			val whereTo = target + move.getOffset
			val boxLeftTarget = whereTo + Coordinates(0,-1)
			move match
				case Down | Up => whereTo.arrayAccess(tiles).canStepOnto(move, whereTo) &&
					boxLeftTarget.arrayAccess(tiles).canStepOnto(move, boxLeftTarget)
				case general.Left => whereTo.arrayAccess(tiles).canStepOnto(move, whereTo)
				case general.Right => whereTo.arrayAccess(tiles).canStepOnto(move, whereTo)
		}

		override def toString: String = "]"
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
		def fromString(s: String): Seq[Tile] = {
			s match
				case "#" => Wall :: Wall :: Nil
				case "O" => BoxLeft :: BoxRight :: Nil
				case "." => Empty :: Empty :: Nil
				case "@" => Robot :: Empty :: Nil
				case _ => throw new IllegalArgumentException()
		}
	}
}

object WareHouse {
	def fromString(input:List[String]):WareHouse = {
		val house = new WareHouse

		house.tiles = input.map(s => s.map(c => c.toString).flatMap(house.Tile.fromString).toArray).toArray

		for (row <- house.tiles.indices;
			 col <- house.tiles.head.indices) {
			if (house.tiles(row)(col) == house.Robot) {
				house.robot = Coordinates(row,col)
			}
		}

		house
	}
}

