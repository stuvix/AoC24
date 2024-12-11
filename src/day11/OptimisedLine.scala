package day11

import scala.collection.mutable

class OptimisedLine {
	private val stoneMap = new mutable.HashMap[Long, Stone]()

	def getStoneAtDepth(stone:Long, depth:Int):Long = {
		getStone(stone).getWidthAtDepth(depth)
	}

	private def getStone(stone:Long):Stone = {
		if (stoneMap.contains(stone)) {
			stoneMap(stone)
		}
		else {
			val newStone = this.Stone(stone, Nil)
			stoneMap(stone) = newStone
			newStone
		}
	}

	def printStoneToDepth(stone:Long, depth:Int):Unit = {
		var stones = getStone(stone) :: Nil
		for (_ <- 0 until depth) {
			stones.foreach(s => print(s.getValue.toString + " "))
			println()
			stones = stones.flatMap(_.children)
		}
	}


	class Stone(value:Long, var children:List[Stone]) {
		private val stored = new mutable.HashMap[Int, Long]()

		def getWidthAtDepth(depth:Int):Long = {
			if (stored.contains(depth)) {
				stored(depth)
			}
			else {
				var ret = 0L
				if (depth == 0) {
					ret = 1
				}
				else {
					assert(depth > 0)
					if (children.isEmpty) {
						if (value == 0) {
							children ::= getStone(1)
						}
						else if (value.toString.length % 2 == 0) {
							children = getStone(value.toString.substring(0, value.toString.length / 2).toLong) ::
								getStone(value.toString.substring(value.toString.length / 2).toLong) ::
								Nil
						}
						else {
							children ::= getStone(value * 2024)
						}
					}
					ret = children.map(_.getWidthAtDepth(depth - 1)).sum
					stored(depth) = ret
				}
				ret
			}
		}

		def getValue:Long = value
	}
}
