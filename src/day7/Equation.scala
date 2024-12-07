package day7

import scala.annotation.tailrec

case class Equation(result:Long, numbers:Array[Int]) {

	def check1: Boolean = {
		recCheck(numbers.head, 0)
	}


	private def recCheck(currentResult:Long, currentIndex:Int): Boolean = {
		if (currentIndex + 1 == numbers.length || currentResult > result) {
			currentResult == result
		}
		else {
			recCheck(currentResult + numbers(currentIndex + 1), currentIndex + 1) ||
				recCheck(currentResult * numbers(currentIndex + 1), currentIndex + 1)
		}
	}

	def check2: Boolean = {
		recCheck2(numbers.head, 0)
	}

	private def recCheck2(currentResult: Long, currentIndex: Int): Boolean = {
		if (currentIndex + 1 == numbers.length || currentResult > result) {
			currentResult == result
		}
		else {
			recCheck2(currentResult + numbers(currentIndex + 1), currentIndex + 1) ||
				recCheck2(currentResult * numbers(currentIndex + 1), currentIndex + 1) ||
				recCheck2((currentResult.toString + numbers(currentIndex + 1).toString).toLong, currentIndex + 1)
		}
	}
}


object Equation  {
	def apply(line:String):Equation = {
		val onColon = line.split(":")
		new Equation(onColon(0).toLong, onColon(1).strip().split("\\s+").map(_.toInt))
	}
}