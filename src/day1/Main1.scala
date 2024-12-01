package day1

import general.*

object Main1 {
	def main(args: Array[String]): Unit = {
		var input = InputReader.getLines("input/day1-test")

		println(calc1(input))
		println(calc2(input))

		input = InputReader.getLines("input/day1")

		println(calc1(input))
		println(calc2(input))
	}

	def calc1(input:List[String]):Int = {
		val tupleList = input.map(s => {
			val split = s.split("\\s+")
			(split(0).toInt, split(1).toInt)
		})
		val (listA:List[Int], listB:List[Int]) = ListOps.splitTupleListIntoLists(tupleList)

		listA.sorted.zip(listB.sorted).map((a,b) => Math.abs(a-b)).sum
	}

	def calc2(input:List[String]):Int = {
		val tupleList = input.map(s => {
			val split = s.split("\\s+")
			(split(0).toInt, split(1).toInt)
		})
		val (listA:List[Int], listB:List[Int]) = ListOps.splitTupleListIntoLists(tupleList)

		var score = 0

		for (n <- listA) {
			score += n * listB.count(_ == n)
		}

		score
	}
}
