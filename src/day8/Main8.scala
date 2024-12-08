package day8

import general.{Coordinates, InputReader, Maths}

object Main8 {

	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day8-test")
		val input = InputReader.getLines("input/day8")

		val map = AntennaMap.fromString(input)
		val res: List[(Coordinates, String)] = map.findAllResonances()
		val count:Int = res.map(_._1).filter(map.isWithinBoard).toSet.size
		println(count)

		val al = map.findAlignments()
		val count2:Int = al.map(_._1).filter(map.isWithinBoard).iterator.toSet.iterator.size
		println(count2)
	}
}