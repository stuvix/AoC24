package day9

import general.{InputReader, RangeList}

import scala.sys.exit

object Main9 {
	def main(args: Array[String]): Unit = {
		//val input = InputReader.getLines("input/day9-test")
		val input = InputReader.getLines("input/day9")


		val memory = Memory.fromString(input.head)
		//println(memory)
		memory.compactify()
		//println(memory)
		println(memory.checksum())

		val blockMemory = BlockMemory.fromString(input.head)
		//println(blockMemory)
		blockMemory.compactify()
		//println(blockMemory)
		println(blockMemory.checksum())



		exit()

		val rangeList = new RangeList[Boolean]
		rangeList.insert(1, 3, true)
		rangeList.foreach(println)
		println()
		rangeList.insert(7, 3, true)
		rangeList.foreach(println)
		println()
		rangeList.insert(18, 3, true)
		rangeList.foreach(println)
		println()
		rangeList.insert(12, 3, true)
		rangeList.foreach(println)
		println()
	}
}
