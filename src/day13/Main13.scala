package day13

import general.InputReader

import scala.util.matching.Regex

object Main13 {
	private val regexButton: Regex = """Button .: X\+(\d+), Y\+(\d+)""".r
	private val regexPrice: Regex = """Prize: X=(\d+), Y=(\d+)""".r

	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day13-test")
		val input = InputReader.getLines("input/day13")

		var claws:List[ClawMachine] = Nil
		var take = input
		while (take.nonEmpty) {
			val current = take.take(4)
			take = take.drop(4)

			var a:Button = null
			var b:Button = null
			current.head match
				case regexButton(x,y) => a = Button(x.toLong, y.toLong)
				case _ => throw new IllegalArgumentException()
			current(1) match
				case regexButton(x, y) => b = Button(x.toLong, y.toLong)
				case _ => throw new IllegalArgumentException()
			current(2) match
				case regexPrice(x,y) => claws ::= new ClawMachine(a,b,x.toLong + 10000000000000L,y.toLong + 10000000000000L)
				case _ => throw new IllegalArgumentException()

			//assert(current(3).matches("\\s*"))
		}

		claws.foreach(println)

		var sum = 0L
		for (claw <- claws) {
			try {
				sum += claw.gaussianElimination
			}
			catch
				case _:AssertionError => // nice "no solution" handling fool
		}

		println(sum)
	}
}
