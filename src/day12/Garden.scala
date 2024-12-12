package day12

import general.{Coordinates, Direction, Up, Right, Down, Left}

import scala.collection.mutable

class Garden {
	var map:Array[Array[String]] = _
	var regions:Array[Array[Region]] = _
	var regionList:List[Region] = Nil

	def fillArrays():Unit = {
		var todo:List[Coordinates] = Coordinates(0,0) :: Nil
		while (todo.nonEmpty) {
			var current = todo.head
			todo = todo.tail
			if (regions(current.row)(current.col) == null) {
				//now we explore this region
				val name = map(current.row)(current.col)
				val region = new Region(name)
				regionList ::= region

				var regionalTodo:List[Coordinates] = current :: Nil
				while (regionalTodo.nonEmpty) {
					current = regionalTodo.head
					regionalTodo = regionalTodo.tail
					if (regions(current.row)(current.col) == null) {
						region.addSquare(current)
						regions(current.row)(current.col) = region
						for (neighbour <- current.getManhattanNeighbours) {
							if (!neighbour.isWithinBoard(regions.length, regions.head.length)) {
								//edge of garden
								region.borderCount += 1
							}
							else if (map(neighbour.row)(neighbour.col) != name) {
								region.borderCount += 1
								todo ::= neighbour
							}
							else if (regions(neighbour.row)(neighbour.col) == null) {
								//unexplored and within region
								regionalTodo ::= neighbour
							}
						}
					}
				}
			}
		}
	}

	def getPrice:Int = {
		regionList.map(r => r.borderCount * r.getSize).sum
	}

	def getCheaperPrice: Int = {
		regionList.map(r => r.useCornerCounting * r.getSize).sum
	}

	class Region(name:String) {
		private var squares:List[Coordinates] = Nil
		var borderCount = 0
		def addSquare(coordinates: Coordinates):Unit = {
			squares ::= coordinates
		}

		def getSize:Int = squares.length

		def countLineBorders:Int = {
			val edges = new mutable.HashMap[Coordinates, List[Direction]]()
			var count = 0

			for (tile <- squares) {
				for (direction <- Direction.getAllDirections) {
					if (!squares.contains(tile + direction.getOffset)) {
						if (!edges.contains(tile)) {
							edges(tile) = Nil
						}
						edges(tile) ::= direction
					}
				}
			}

			// we count corners, as there are as many corners as there are flat sides

			// find all corners


			count
		}

		def useCornerCounting:Int = {
			var corners = 0

			for (square <- squares) {
				if (!squares.contains(square + Up.getOffset) && !squares.contains(square + Right.getOffset)) {
					corners += 1
				}
				if (!squares.contains(square + Right.getOffset) && !squares.contains(square + Down.getOffset)) {
					corners += 1
				}
				if (!squares.contains(square + Down.getOffset) && !squares.contains(square + Left.getOffset)) {
					corners += 1
				}
				if (!squares.contains(square + Left.getOffset) && !squares.contains(square + Up.getOffset)) {
					corners += 1
				}

				if (squares.contains(square + Up.getOffset) && squares.contains(square + Right.getOffset) &&
					!squares.contains(square + Up.getOffset + Right.getOffset)) {
					corners += 1
				}
				if (squares.contains(square + Right.getOffset) && squares.contains(square + Down.getOffset) &&
					!squares.contains(square + Right.getOffset + Down.getOffset)) {
					corners += 1
				}
				if (squares.contains(square + Down.getOffset) && squares.contains(square + Left.getOffset) &&
					!squares.contains(square + Down.getOffset + Left.getOffset)) {
					corners += 1
				}
				if (squares.contains(square + Left.getOffset) && squares.contains(square + Up.getOffset) &&
					!squares.contains(square + Left.getOffset + Up.getOffset)) {
					corners += 1
				}
			}

			corners
		}
	}
}

object Garden {
	def fromString(list:List[String]):Garden = {
		val garden = new Garden()
		garden.map = list.map(_.toCharArray.map(_.toString)).toArray
		garden.regions = Array.ofDim[garden.Region](garden.map.length, garden.map.head.length)
		garden
	}
}


