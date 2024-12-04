package day4

class Grid {
	var grid:Array[Array[String]] = _

	def get(row:Int, col:Int):String = grid(row)(col)

	def safeGet(row:Int, col:Int):String = {
		if (row >= 0 && row < grid.length && col >= 0 && col < grid(0).length) {
			get(row,col)
		}
		else {
			"$"
		}
	}

	def searchForGivenX(row:Int, col:Int):Int = {
		var count = 0
		assert(this.get(row, col) == "X")
		if (safeGet(row,col-1) == "M" && safeGet(row,col-2) == "A" && safeGet(row,col-3) == "S") {
			count += 1
		}
		if (safeGet(row,col+1) == "M" && safeGet(row,col+2) == "A" && safeGet(row,col+3) == "S") {
			count += 1
		}
		if (safeGet(row-1,col) == "M" && safeGet(row-2,col) == "A" && safeGet(row-3,col) == "S") {
			count += 1
		}
		if (safeGet(row+1,col) == "M" && safeGet(row+2,col) == "A" && safeGet(row+3,col) == "S") {
			count += 1
		}

		if (safeGet(row-1,col-1) == "M" && safeGet(row-2,col-2) == "A" && safeGet(row-3,col-3) == "S") {
			count += 1
		}
		if (safeGet(row+1,col-1) == "M" && safeGet(row+2,col-2) == "A" && safeGet(row+3,col-3) == "S") {
			count += 1
		}
		if (safeGet(row-1,col+1) == "M" && safeGet(row-2,col+2) == "A" && safeGet(row-3,col+3) == "S") {
			count += 1
		}
		if (safeGet(row+1,col+1) == "M" && safeGet(row+2,col+2) == "A" && safeGet(row+3,col+3) == "S") {
			count += 1
		}
		count
	}

	def searchForGivenA(row:Int, col:Int):Int = {
		var count = 0
		assert(this.get(row, col) == "A")
		if (safeGet(row-1,col+1) == "M" && safeGet(row+1,col-1) == "S") {
			count += 1
		}
		if (safeGet(row-1,col+1) == "S" && safeGet(row+1,col-1) == "M") {
			count += 1
		}

		if (safeGet(row+1,col+1) == "M" && safeGet(row-1,col-1) == "S") {
			count += 1
		}
		if (safeGet(row+1,col+1) == "S" && safeGet(row-1,col-1) == "M") {
			count += 1
		}
		if count == 2 then 1 else 0
	}

	def findAllLetters(letter:String):Iterable[(Int,Int)] = {
		var xs:List[(Int, Int)] = Nil
		for (row <- grid.indices) {
			for (col <- grid(row).indices) {
				if (get(row,col) == letter) {
					xs ::= (row,col)
				}
			}
		}
		xs.reverse
	}
}

object Grid {
	def apply(input:List[String]): Grid = {
		val g = new Grid()
		g.grid = new Array[Array[String]](input.length)
		var lineNumber = 0
		for (line <- input) {
			g.grid(lineNumber) = line.map(_.toString).toArray
			lineNumber += 1
		}
		g
	}
}
