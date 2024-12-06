package day6

import general.InputReader

import scala.util.control.Breaks.*
import scala.util.control.NonLocalReturns.*

object Main6 {
	def main(args: Array[String]): Unit = {
		//var input = InputReader.getLines("input/day6-test")
		var input = InputReader.getLines("input/day6")

		val tiles = input.map(_.map(letter => Tile.fromString(letter.toString)).toArray).toArray

		var guardX = 0
		var guardY = 0
		for (lineNum <- input.indices) {
			if (input(lineNum).contains("^")) {
				guardY = lineNum
				guardX = input(lineNum).indexOf("^")
				assert(guardX >= 0)
			}
		}

		doMoves(tiles, guardY, guardX)

		//printBoard(tiles)

		//println(tiles.map(row => row.count(_.hasBeenVisited)).sum)

		println(replaceAllTilesWithObstacles(tiles, guardY, guardX))
	}

	def doMoves(tiles:Array[Array[Tile]], guardRow:Int, guardCol:Int):Unit = {
		var pos = (guardRow, guardCol)
		var dir:Direction = Up
		var notLeft = true

		while (notLeft) {
			tiles(pos._1)(pos._2).visit()
			val newPos = dir.applyMove(pos)
			if (!withinBoard(newPos, tiles.length, tiles.head.length)) {
				notLeft = false
			}
			else {
				if (tiles(newPos._1)(newPos._2).isInstanceOf[Obstacle]) {
					dir = dir.turnRight()
				}
				else {
					pos = newPos
				}
			}
		}
	}

	def withinBoard(pos:(Int, Int), rows:Int, cols:Int):Boolean = {
		pos._1 >= 0 && pos._1 < rows && pos._2 >= 0 && pos._2 < cols
	}

	def printBoard(tiles:Array[Array[Tile]]):Unit = {
		for (line <- tiles) {
			line.foreach(print)
			println()
		}
	}

	def replaceAllTilesWithObstacles(tiles:Array[Array[Tile]], guardRow:Int, guardCol:Int): Int = {
		var loops = 0
		for (row <- tiles.indices) {
			for (col <- tiles(row).indices) {
				if (tiles(row)(col).isInstanceOf[Ground] && !(guardRow == row && guardCol == col)) {
					tiles(row)(col) = new day6.Obstacle
					if (doMovesUntilLoop(tiles, guardRow, guardCol)) {
						loops += 1
					}
					tiles(row)(col) = new Ground
				}
				//now reset
				for (row <- tiles) {
					for (t <- row) {
						t.reset()
					}
				}
			}
		}
		loops
	}

	// true if loop
	def doMovesUntilLoop(tiles:Array[Array[Tile]], guardRow:Int, guardCol:Int):Boolean = {
		returning {
			var pos = (guardRow, guardCol)
			var dir:Direction = Up
			var notLeft = true

			while (notLeft) {
				if(tiles(pos._1)(pos._2).visit(dir)) {
					throwReturn(true)
				}
				val newPos = dir.applyMove(pos)
				if (!withinBoard(newPos, tiles.length, tiles.head.length)) {
					notLeft = false
				}
				else {
					if (tiles(newPos._1)(newPos._2).isInstanceOf[Obstacle]) {
						dir = dir.turnRight()
					}
					else {
						pos = newPos
					}
				}
			}
			false
		}

	}
}
