package day8

import general.{Coordinates, ListOps}

import scala.collection.mutable

class AntennaMap(width:Int, height:Int) {
	var antennas = new mutable.HashMap[String, List[Coordinates]]()
	
	def findAllResonances():List[(Coordinates, String)] = {
		var ret:List[(Coordinates, String)] = Nil
		for ((name, coordinates) <- antennas) {
			val pairs = ListOps.generateAllPairs(coordinates)
			for ((a,b) <- pairs
				 if a != b) {
				ret ::= (a.getResonantLocationOutside(b), name)
				ret ::= (b.getResonantLocationOutside(a), name)
				a.getResonantLocationInside(b) match
					case Some(value) => ret ::= (value, name)
					case None => //do nothing
				b.getResonantLocationInside(a) match
					case Some(value) => ret ::= (value, name)
					case None => //do nothing
			}
		}
		ret
	}
	
	def isWithinBoard(coordinates: Coordinates):Boolean = {
		(coordinates.row >= 0 && coordinates.col >= 0) &&
			(coordinates.row < height && coordinates.col < width)
	}
	
	def findAlignments():List[(Coordinates, String)] = {
		var ret: List[(Coordinates, String)] = Nil
		for ((name, coordinates) <- antennas) {
			val pairs = ListOps.generateAllPairs(coordinates)
			for ((a, b) <- pairs
				 if a != b) {
				val delta = a.getSmallestIntegerDelta(b)
				var n = 0
				while (isWithinBoard(a + (delta * n))) {
					ret ::= (a + (delta * n), name)
					n += 1
				}
				n = -1
				while (isWithinBoard(a + (delta * n))) {
					ret ::= (a + (delta * n), name)
					n -= 1
				}
			}
		}
		ret
	}
}

object AntennaMap {
	def fromString(input:List[String]):AntennaMap = {
		val map = new AntennaMap(input.head.length, input.length)
		for (row <- input.indices) {
			for (col <- input(row).indices) {
				val key = input(row).charAt(col).toString
				if (!key.equals(".")) {
					if (!map.antennas.contains(key)) {
						map.antennas(key) = Nil
					}
					map.antennas(key) ::= Coordinates(row,col)
				}			
			}
		}
		map
	}
}
