package day22

import general.ListOps

import scala.collection.mutable



class SecretNumber {
  import SecretNumber.{prune, mix}

  private var secret:Long = 0
  private var sequence:List[Int] = Nil
  private var diffList:List[Int] = Nil
  var sequenceValues = new mutable.HashMap[(Int, Int, Int, Int), Long]()

  def step(n:Int = 1):Unit = {
    sequence ::= (secret%10).toInt
    for (_ <- 0 until n) {
      secret = prune(mix(secret, secret * 64))
      secret = prune(mix(secret, secret / 32))
      secret = prune(mix(secret, secret * 2048))
      sequence ::= (secret%10).toInt
    }
    sequence = sequence.reverse
  }

  def makeDiffList():Unit =  {
    diffList = ListOps.consecutiveTuples(sequence).map(tuple => tuple._2 - tuple._1)
  }

  def fillSequenceValues():Unit = {
    var d = diffList
    var s = sequence.drop(4)
    while (d.length >= 4) {
      val current = d.take(4)
      d = d.tail
      val price = s.head
      s = s.tail
      val tuple = (current.head, current(1), current(2), current(3))
      if (!sequenceValues.contains(tuple)) {
        sequenceValues.put(tuple, price)
      }
      else {
        /*if (sequenceValues(tuple) < price) {
          sequenceValues(tuple) = price
        }*/
      }
    }
    assert(s.isEmpty)
  }

  def print:Unit = println(secret)

  def number:Long = secret
}


object SecretNumber {
  def mix(secret:Long, other:Long):Long = {
    secret ^ other
  }

  def prune(secret:Long):Long = {
    secret % 16777216L
  }

  def apply(n:Long):SecretNumber = {
    val s = new SecretNumber
    s.secret = n
    s
  }
}