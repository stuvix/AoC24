package day7

import general.InputReader

object Main7 {
	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day7-test")
		val input = InputReader.getLines("input/day7")

		val eqs = input.map(Equation.apply)

		//eqs.foreach(println)
		println(eqs.filter(_.check1).map(e => e.result).sum)
		println(eqs.filter(_.check2).map(e => e.result).sum)
	}

}


