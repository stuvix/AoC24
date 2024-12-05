package day5

import general.InputReader
import scala.util.control.Breaks.*

object Main5 {
	def main(args: Array[String]): Unit = {
		//var input = InputReader.getLines("input/day5-test")
		var input = InputReader.getLines("input/day5")

		var rules:List[Rule] = Nil
		var orders:List[List[Int]] = Nil

		for (line <- input) {
			if (line.contains("|")) {
				rules ::= Rule(line)
			}
			else if (line.contains(",")) {
				orders ::= line.split(",").map(_.toInt).toList
			}
			else {
				//println("invalid line")
			}
		}
		rules = rules.reverse
		orders = orders.reverse

		//rules.foreach(println)
		//orders.foreach(println)

		var sum = 0
		var obeys = true

		var disobeyers:List[List[Int]] = Nil

		for (order <- orders) {
			breakable {
				obeys = true
				for (rule <- rules) {
					if (!rule.obeys(order)) {
						//println(f"order ${order} does not obey rule ${rule}")
						obeys = false
						disobeyers ::= order
						break
					}
				}
				if (obeys) {
					println(order(order.length / 2 ))
					sum += order(order.length / 2 )
				}
			}

		}

		println(sum)

		sum = 0
		for (order <- disobeyers) {
			var startingOrder = order

			var hasChanged = true
			while (hasChanged) {
				breakable {
					hasChanged = false
					for (rule <- rules) {
						if (!rule.obeys(startingOrder)) {
							startingOrder = rule.swap(startingOrder)
							hasChanged = true
							break
						}
					}
				}

			}


			//println(startingOrder)
			//println(startingOrder(startingOrder.length / 2 ))
			sum += startingOrder(startingOrder.length / 2 )
		}

		println(sum)
	}
}
