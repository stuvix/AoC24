package day9

import scala.util.control.Breaks.breakable

class Memory {
	var bytes:Array[Int] = _


	override def toString: String = {
		bytes.map(x => if x > -1 then x.toString else ".").foldLeft("")(_ + _)
	}

	def compactify():Unit = {
		var firstFree = bytes.indexOf(-1)
		var workingOn = bytes.length - 1
	
		while (firstFree < workingOn) {
			bytes(firstFree) = bytes(workingOn)
			bytes(workingOn) = -1
			while (bytes(workingOn) == -1) {
				workingOn -= 1
			}
			while (bytes(firstFree) != -1) {
				firstFree += 1
			}
		}
	}
	
	def checksum():Long = {
		var sum:Long = 0
		for (index <- bytes.indices) {
			sum += (if bytes(index) == -1 then 0 else index * bytes(index))
		}
		sum
	}
}

object Memory {
	def fromString(line:String):Memory = {
		val memory = new Memory()

		memory.bytes = new Array[Int](line.toCharArray.map(_.toString.toInt).sum)

		var fileIndex = 0
		var isFile = true
		var index = 0
		for (d <- line.toCharArray.map(_.toString.toInt)) {
			for (i <- 0 until d) {
				memory.bytes(index) = if isFile then fileIndex else -1
				index += 1
			}
			if isFile then fileIndex += 1
			isFile = !isFile
		}
		assert(index == memory.bytes.length)

		memory
	}
}
