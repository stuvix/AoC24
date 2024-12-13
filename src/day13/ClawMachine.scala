package day13

import scala.annotation.targetName

class ClawMachine(a:Button, b:Button, x:Long, y:Long) {

	// equation 1: a.x * aCount + b.x * bCount = x
	// equation 2: a.y * aCount + b.y * bCount = y
	// minimise  : 3 * aCount + bCount

	override def toString: String = {
		f"$a \n$b \nPrice: $x, $y\n"
	}


	def gaussianElimination:Long = {
		var result = 0L

		var first = Equation(a.x, b.x, x) * a.y
		var second = Equation(a.y, b.y, y)
		var eliminated = first + (second * (-a.x))
		assert(eliminated.a == 0)

		var aCount = 0L
		var bCount = 0L

		if (eliminated.b == 0) {
			//is this the case if we only have one actual equation?
			bCount = 0
		}
		else {
			assert(eliminated.r % eliminated.b == 0) // if this is not the case, this has no solution?
			bCount = eliminated.r / eliminated.b
		}
		assert((first.r - (first.b * bCount)) % first.a == 0) // if this is not the case, this has no solution?
		aCount = (first.r - (first.b * bCount)) / first.a

		//println(f"$aCount * ${a.x} + $bCount * ${b.x} = ${aCount * a.x + bCount * b.x}")
		//println(f"$aCount * ${a.y} + $bCount * ${b.y} = ${aCount * a.y + bCount * b.y}")

		result = 3 * aCount + 1 * bCount
		result
	}
}


case class Button(x:Long, y:Long) {

	@targetName("scalar multiplication")
	def *(n:Int):Button = {
		Button(x*n, y*n)
	}

	@targetName("add")
	def +(button: Button):Button = {
		Button(this.x+button.x, this.y+button.y)
	}
}

case class Equation(a:Long, b:Long, r:Long) {
	@targetName("scalar multiplication")
	def *(n: Long): Equation = {
		Equation(a * n, b * n, r * n)
	}

	@targetName("add")
	def +(eq: Equation): Equation = {
		Equation(this.a + eq.a, this.b + eq.b, this.r + eq.r)
	}
}