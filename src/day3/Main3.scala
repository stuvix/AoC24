package day3

import general.InputReader
import scala.util.matching.Regex

object Main3 {
	val valid1:Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
	val valid2:Regex = """(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))""".r

	def main(args: Array[String]): Unit = {
		var input = InputReader.getLines("input/day3-test")

		println(doStuff1(input.head))
		println(doStuff2(input.head))

		input = InputReader.getLines("input/day3")

		println(input.map(doStuff1).sum)
		println(doStuff2(input.foldLeft("")(_ + _)))
	}

	def doStuff1(s:String):Int = {
		valid1.findAllMatchIn(s).map(m => m.group(1).toInt * m.group(2).toInt).sum
	}

	def doStuff2(s:String):Int = {
		var active = true
		valid2.findAllMatchIn(s).map(m => {
			m.toString() match
				case valid1(a,b) => if active then a.toInt * b.toInt else 0
				case "do()" => {active = true;0}
				case "don't()" => {active = false;0}
		}).sum
	}
}

