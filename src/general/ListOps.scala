package general

object ListOps {
	def splitTupleListIntoLists[A, B](list: List[(A, B)]): (List[A], List[B]) = {
		var listA: List[A] = Nil
		var listB: List[B] = Nil

		for ((a, b) <- list) {
			listA ::= a
			listB ::= b
		}

		(listA.reverse, listB.reverse)
	}
}
