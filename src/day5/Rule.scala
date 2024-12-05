package day5

import scala.util.control.NonLocalReturns.*

case class Rule(before:Int, after:Int) {
	def obeys(order:List[Int]):Boolean = {
		returning{
			var encounteredBefore = false
			var encounteredAfter = false
			for (page <- order) {
				if (page == before) {
					encounteredBefore = true
				}
				if (page == after) {
					encounteredAfter = true
				}
				if (page == before && encounteredAfter) {
					throwReturn(false)
				}
			}
			true
		}
	}

	def swap(order:List[Int]):List[Int] = {
		val array = order.toArray
		val indexA = order.indexOf(before)
		val indexB = order.indexOf(after)
		val temp = array(indexA)
		array(indexA) = array(indexB)
		array(indexB) = temp
		array.toList
	}
}

object Rule {
	def apply(line:String):Rule = {
		val split= line.split("\\|")
		new Rule(split(0).toInt, split(1).toInt)
	}
}
