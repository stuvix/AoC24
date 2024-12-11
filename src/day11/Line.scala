package day11

import scala.collection.mutable.ListBuffer

case class Line(var stones:List[Stone] = Nil) {
	def blink:Line = {
		val buffer = new ListBuffer[Stone]()
		for (stone <- stones) {
			buffer ++= stone.blink
		}
		Line(buffer.toList)
	}
}


case class Stone(value: Long) {
	def blink:List[Stone] = {
		var stones:List[Long] = Nil

		if (value == 0)
			stones = 1 :: Nil
		else if (value.toString.length % 2 == 0)
			 val s = value.toString
			 stones = s.substring(0, s.length / 2).toLong :: s.substring(s.length / 2).toLong :: Nil
		else
			stones = value * 2024 :: Nil

		stones.map(Stone(_))
	}
}