package day14

import general.InputReader

import scala.util.matching.Regex

object Main14 {
	val roboting: Regex = """p=(-?\d+),(-?\d+)\s+v=(-?\d+),(-?\d+)\s*""".r

	def main(args: Array[String]): Unit = {
		val testing = false
		var input:List[String] = null
		var field:Field = null

		if (testing) {
			input = InputReader.getLines("input/day14-test")
			field = Field(11, 7)
		}
		else {
			input = InputReader.getLines("input/day14")
			field = Field(101,103)
		}

		//input = "p=2,4 v=2,-3" :: Nil


		for (line <- input) {
			line match
				case roboting(x,y,dx,dy) => field.addRobot(y.toInt,x.toInt,dy.toInt,dx.toInt)
				case _ => throw new IllegalArgumentException()
		}

/*
		println(field)
		field.advanceAllRobots(1)
		println(field)
		field.advanceAllRobots(1)
		println(field)
		field.advanceAllRobots(1)
		println(field)
		field.advanceAllRobots(1)
		println(field)
		field.advanceAllRobots(1)

		sys.exit()

*/
		/*
		field.advanceAllRobots(100)
		field.getQuadrantSecurityValues.foreach(println)
		println(field.getQuadrantSecurityValues.product)
	*/	
		for (i <- 0 until 103*101) {
			if (field.countStacked < 2) {
				println(i)
				println(field)
			}
			field.advanceAllRobots(1)
		}

		//println(field)

	}
}