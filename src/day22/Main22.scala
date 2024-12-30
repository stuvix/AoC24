package day22

import general.{InputReader, ListOps}

import scala.collection.mutable

object Main22 {
  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null

    if (testing) {
      input = InputReader.getLines("input/day22-test")

    }
    else {
      input = InputReader.getLines("input/day22")

    }


    var sum = 0L
    val map = new mutable.HashMap[(Int, Int, Int, Int), Long]()

    //input = "123" ::Nil
    val iterations = 2000

    for (line <- input) {
      val s = SecretNumber(line.toLong)
      s.step(iterations)
      sum += s.number
      s.makeDiffList()
      s.fillSequenceValues()
      addMaps(map, s.sequenceValues)
    }
    println(sum)
    var (seq, max) = ((0,0,0,0), -1L)
    for ((key, value) <- map) {
      if (value > max) {
        println(value)
        max = value
        seq = key
      }
    }
    println(seq)
    println(max) //1647 is too much //1577 is too low
    println(map.size)
  }

  private def addMaps(main:mutable.HashMap[(Int, Int, Int, Int), Long], other:mutable.HashMap[(Int, Int, Int, Int), Long]):Unit = {
    for ((key, value) <- other) {
      if (!main.contains(key)) {
        main.put(key, 0)
      }
      main(key) += value
    }
  }
}
