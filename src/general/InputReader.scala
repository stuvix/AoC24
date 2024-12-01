package general

import scala.io.Source

object InputReader {

	def getLines(fileName: String): List[String] = {
		val source = Source.fromFile(fileName)
		val list = source.getLines().toList
		source.close()
		list
	}
}
