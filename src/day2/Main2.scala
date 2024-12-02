package day2

import general.{InputReader, ListOps}

object Main2 {
	def main(args: Array[String]): Unit = {
		var input = InputReader.getLines("input/day2-test")
		var count = 0
		count = input.map(_.split("\\s+").map(_.toInt))count(arr => isLineSafeDamp(arr.toList))
		println(count)

		input = InputReader.getLines("input/day2")
		count = input.map(_.split("\\s+").map(_.toInt)).count(arr => isLineSafeDamp(arr.toList))
		println(count)
	}


	def isLineSafe(line:List[Int]):Boolean = {
		var current = line.head
		var ret = true
		val decreasing = if current > line.tail.head then true else false
		for (x <- line.tail) {
			if (decreasing) {
				if (!(current > x && current - 4 < x)) {
					ret = false
				}
			}
			else {
				if (!(current < x && current + 4 > x)) {
					ret = false
				}
			}
			current = x
		}
		ret
	}
	
	def isLineSafeDamp(line:List[Int]):Boolean = {
		if (isLineSafe(line)) {
			true
		}
		else {
			ListOps.removeEachElementOnce(line).map(isLineSafe).count(x => x) > 0
		}
	}
}
