package general

object Maths {
	def lcm(list: Seq[Long]): Long = list.foldLeft(1: Long) {
		(a, b) =>
			b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
	}

	def findCommonFactors(b:Seq[Int]):Seq[Int] = {
		assert(b.nonEmpty)
		(1 to b.max).filter { x => b.forall { z => z % x == 0 } }
	}
}
