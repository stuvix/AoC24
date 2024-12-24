package day19

import general.InputReader

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.matching.Regex

object Main19 {
  private var towels:Array[String] = uninitialized

  def main(args: Array[String]): Unit = {
    val testing = false
    var input: List[String] = null

    if (testing) {
      input = InputReader.getLines("input/day19-test")

    }
    else {
      input = InputReader.getLines("input/day19")

    }

    val towels = input.head
    val needed = input.tail.tail

    val regex = buildRegex(towels)
    var count = 0L
    for (need <- needed) {
      if (regex.pattern.matcher(need).matches())
        count += 1
    }
    println(count)

    Main19.towels = towels.split(",\\s*")

    count = 0
    for (need <- needed) {
      count += countPossibilities(need)
    }
    println(count)
  }

  private def buildRegex(towels:String):Regex = {
    val center = towels.split(",\\s*").map(s => f"($s)").mkString("|")
    f"($center)*".r
  }

  private val cache = new mutable.HashMap[String, Long]()

  private def countPossibilities(needed:String):Long = {
    if (cache.contains(needed)) {
      cache(needed)
    }
    else if (needed == "") {
      1L
    }
    else {
      var count = 0L
      for (towel <- towels) {
        if (needed.startsWith(towel)) {
          count += countPossibilities(needed.substring(towel.length))
        }
      }
      cache.addOne((needed, count))
      count
    }
  }
}
