package day15

import general.{Direction, InputReader}

object Main15 {
	def main(args: Array[String]): Unit = {
		val testing = true
		var input:List[String] = null

		if (testing) {
			input = InputReader.getLines("input/day15-test")

		}
		else {
			input = InputReader.getLines("input/day15")

		}

		var map:List[String] = Nil
		var moves = ""
		var encounteredBlank = false

		for (line <- input) {
			if (line.matches("\\s*")) {
				encounteredBlank = true
			}
			else {
				if (!encounteredBlank) {
					map ::= line
				}
				else {
					moves += line
				}
			}
		}
		map = map.reverse

		//map.foreach(println)

		val wareHouse = WareHouse.fromString(map)
		wareHouse.executeMoves(moves.toCharArray.map(_.toString).map(Direction.fromString))
		println(wareHouse.getBoxCoordinates.map(c => c.row * 100 + c.col * 1).sum)

		//println(wareHouse)
	}
}
