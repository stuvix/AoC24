package day17

import general.InputReader

object Main17 {
  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null

    if (testing) {
      input = InputReader.getLines("input/day17-test")

    }
    else {
      input = InputReader.getLines("input/day17")

    }

    var (a:Int, b:Int, c:Int, list:String) = (0,0,0,"")

    for (line <- input) {
      line match
        case RA(x) => a = x.toInt
        case RB(x) => b = x.toInt
        case RC(x) => c = x.toInt
        case pr(x) => list = x
        case _ => //skip
    }

    val computer = Computer(a,b,c, list.split(",").map(_.toInt))

    computer.A = 106086382266778L

    computer.run()
    println(computer.output.reverse.mkString(","))
    println(computer.memory.mkString(","))

    val wanted = computer.memory.reverse
    println(doThing(computer, wanted))
  }

  private val RA = """Register A: (\d+)""".r
  private val RB = """Register B: (\d+)""".r
  private val RC = """Register C: (\d+)""".r
  private val pr = """Program: (.+)""".r


  def doThing(computer:Computer, wanted:Seq[Int], solution:Long = 0L):(Long, Boolean) = {
    if (wanted.isEmpty) {
      (solution, true)
    }
    else {
      computer.abortOnJump = true
      val current = wanted.head
      var solutionList: List[Long] = Nil

      for (i <- 0 until 8) {
        val attempt = solution * 8 + i
        computer.RIP = 0
        computer.A = attempt
        computer.output = Nil
        computer.run()
        if (computer.output.head == current) {
          val sol = doThing(computer, wanted.tail, attempt)
          if sol._2 then solutionList ::= sol._1
        }
      }

      if solutionList.isEmpty then (0L, false) else (solutionList.min, true)
    }
  }
}

