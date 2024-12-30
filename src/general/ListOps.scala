package general

import scala.collection.mutable.ListBuffer

object ListOps {

	/**
	 * takes a list with tuples in it and returns two new lists, one for each element of the tuple. Kinda like unzip
	 * unzip exists -_-
	 * @return
	 */
	def splitTupleListIntoLists[A, B](list: List[(A, B)]): (List[A], List[B]) = {
		var listA: List[A] = Nil
		var listB: List[B] = Nil

		for ((a, b) <- list) {
			listA ::= a
			listB ::= b
		}

		(listA.reverse, listB.reverse)
	}

	/**
	 * creates list.length many new lists from one original list: each new list is one element shorter than the input
	 * and contains all elements in order, except one.
	 */
	def removeEachElementOnce[A](list:List[A]):List[List[A]] = {
		var clones:List[List[A]] = Nil
		for (i <- list.indices) {
			val newList = new ListBuffer[A]()
			newList.addAll(list)
			newList.remove(i)
			clones ::= newList.toList
		}
		clones
	}
	
	def generateAllPairs[A](list:List[A]):List[(A,A)] = {
		for (x <- list; y <- list) 
			yield (x,y)
	}

	def allPermutationsOfList[T](list:List[T]):List[List[T]] = {
		list.permutations.toList
	}
	
	def consecutiveTuples[T](list:List[T]):List[(T,T)] = {
		list.dropRight(1).zip(list.tail)
	}
}
