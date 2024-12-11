package day11

import general.InputReader

import scala.collection.immutable.HashSet

object Main11 {
	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day11-test")
		val input = InputReader.getLines("input/day11")

		var stones = Line(input.head.split("\\s+").map(x => Stone(x.toLong)).toList)
		var previous = 0
		var set:HashSet[Long] = new HashSet[Long]()
		for (i <- 0 until 25) {
			previous = set.size
			//println(f"$i: ${stones.stones.length}")
			stones = stones.blink
			//println(f"diff: ${stones.stones.length - previous}")
			//println(f"unique stones: ${stones.stones.toSet.iterator.length}")
			set ++= stones.stones.map(_.value)
			//println(f"new stones: ${set.size - previous}")
		}
		println(stones.stones.length)


		println("------------------")
		val opt = new OptimisedLine()
		println(input.head.split("\\s+").map(_.toLong).map(opt.getStoneAtDepth(_, 75)).sum)

		//opt.printStoneToDepth(125, 6)

		println("------------------")
	}
}
