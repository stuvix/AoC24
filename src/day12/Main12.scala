package day12

import general.InputReader

object Main12 {
	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day12-test")
		val input = InputReader.getLines("input/day12")

		val garden = Garden.fromString(input)
		garden.fillArrays()
		println(garden.getPrice)
		println(garden.getCheaperPrice)
	}
}
