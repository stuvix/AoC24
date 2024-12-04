package day4

import general.InputReader

object Main4 {
	def main(args: Array[String]): Unit = {
		var input = InputReader.getLines("input/day4-test")

		var grid = Grid(input)
		println(grid.findAllLetters("X").map((row, col) => grid.searchForGivenX(row,col)).sum)
		println(grid.findAllLetters("A").map((row, col) => grid.searchForGivenA(row,col)).sum)

		input = InputReader.getLines("input/day4")

		grid = Grid(input)
		println(grid.findAllLetters("X").map((row, col) => grid.searchForGivenX(row,col)).sum)
		println(grid.findAllLetters("A").map((row, col) => grid.searchForGivenA(row,col)).sum)
	}


}
