package general

import scala.annotation.targetName

case class Coordinates(row:Int, col:Int) {
	def manhattanDistanceTo(other:Coordinates):Int = {
		Math.abs(this.row - other.row) + Math.abs(this.col - other.col)
	}

	def areAlignedFromHere(a:Coordinates, b:Coordinates):Boolean = {
		val deltaRow1 = a.row - this.row
		val deltaCol1 = a.col - this.col
		val deltaRow2 = b.row - this.row
		val deltaCol2 = b.col - this.col

		deltaRow1 * deltaCol2 == deltaRow2 * deltaCol1
	}

	def getResonantLocationOutside(other:Coordinates):Coordinates = {
		val delta = other - this
		other + delta
	}

	def getResonantLocationInside(other: Coordinates): Option[Coordinates] = {
		val delta = other - this
		if (delta.row % 3 == 0 && delta.col % 3 == 0) {
			Some(this + Coordinates(delta.row / 3, delta.col / 3))
		}
		else {
			None
		}
	}

	/**
	 * to find all aligned positions on the grid, calculate "this + n * result"
	 */
	def getSmallestIntegerDelta(other:Coordinates):Coordinates = {
		val delta = other - this
		if (delta.row == 0) {
			Coordinates(0,1)
		}
		else if (delta.col == 0) {
			Coordinates(1,0)
		}
		else {
			val common = Maths.findCommonFactors(Math.abs(delta.row) :: Math.abs(delta.col) :: Nil).iterator.max
			delta / common
		}
	}
	
	def getManhattanNeighbours:List[Coordinates] = {
		this + Coordinates(1,0) :: this + Coordinates(0,1) :: this + Coordinates(-1,0) :: this + Coordinates(0,-1) :: Nil
	}
	
	def getDiagonalNeighbours:List[Coordinates] = {
		this + Coordinates(1, 1) :: this + Coordinates(-1, 1) :: this + Coordinates(-1, -1) :: this + Coordinates(1, -1) :: Nil
	}
	
	def isWithinBoard(rowCount:Int, colCount:Int):Boolean = {
		row >= 0 && col >= 0 &&
			row < rowCount && col < colCount
	}
	
	def arrayAccess[T](array:Array[Array[T]]):T = {
		array(row)(col)
	}
	
	def arrayWrite[T](array:Array[Array[T]], x:T):Unit = {
		array(row)(col) = x
	}
	
	def reverse:Coordinates = Coordinates(col,row)

	@targetName("add")
	def +(other:Coordinates):Coordinates = {
		Coordinates(this.row + other.row, this.col + other.col)
	}

	@targetName("sub")
	def -(other: Coordinates): Coordinates = {
		Coordinates(this.row - other.row, this.col - other.col)
	}


	@targetName("scalarMultiplication")
	def *(scalar:Int):Coordinates = {
		Coordinates(row * scalar, col * scalar)
	}

	@targetName("scalarDivision")
	def /(scalar: Int): Coordinates = {
		Coordinates(row / scalar, col / scalar)
	}
}


object Coordinates {
	def fromString(string:String):Coordinates = {
		val list = string.trim.split(",")
		assert(list.length == 2)
		Coordinates(list.head.toInt, list.tail.head.toInt)
	}
}