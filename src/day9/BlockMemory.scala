package day9

import general.RangeList


class BlockMemory {
	val freeList = new RangeList[Boolean]
	val blockList = new RangeList[Int]

	override def toString: String = {
		val array = new Array[String](freeList.blockLength + blockList.blockLength)
		for (element <- freeList) {
			for (i <- element.start until element.start + element.size) {
				array(i) = "."
			}
		}
		for (element <- blockList) {
			for (i <- element.start until element.start + element.size) {
				array(i) = element.payload.toString
			}
		}
		array.foldLeft("")(_ + _)
	}

	def compactify():Unit = {
		var fileIndex = blockList.last.payload
		while (fileIndex > 0) {
			val currentFile = blockList.getForPayload(fileIndex).get
			val moveTo = freeList.findFirstOfMinSize(currentFile.size)
			if (moveTo.nonEmpty && moveTo.get.start < currentFile.start) {
				blockList.remove(currentFile)
				freeList.insert(currentFile.start, currentFile.size, true)
				blockList.insert(moveTo.get.start, currentFile.size, currentFile.payload)
				freeList.remove(moveTo.get)
				if (moveTo.get.size > currentFile.size) {
					freeList.insert(moveTo.get.start + currentFile.size, moveTo.get.size - currentFile.size, true)
				}
			}
			fileIndex -= 1
		}
	}

	def checksum():Long = {
		val array = new Array[Int](freeList.blockLength + blockList.blockLength)
		for (element <- freeList) {
			for (i <- element.start until element.start + element.size) {
				array(i) = -1
			}
		}
		for (element <- blockList) {
			for (i <- element.start until element.start + element.size) {
				array(i) = element.payload
			}
		}

		var sum: Long = 0
		for (index <- array.indices) {
			sum += (if array(index) == -1 then 0 else index * array(index))
		}
		sum
	}
}


object BlockMemory {
	def fromString(line:String):BlockMemory = {
		val blockMemory = new BlockMemory

		var fileIndex = 0
		var isFile = true
		var index = 0
		for (d <- line.toCharArray.map(_.toString.toInt)) {
			if (d > 0) {
				if (isFile) {
					blockMemory.blockList.insert(index, d, fileIndex)
				}
				else {
					blockMemory.freeList.insert(index, d, true)
				}
			}
			index += d
			if isFile then fileIndex += 1
			isFile = !isFile
		}


		blockMemory
	}
}

