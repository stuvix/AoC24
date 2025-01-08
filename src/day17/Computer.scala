package day17

import scala.compiletime.uninitialized

class Computer {
  var A:Long = uninitialized
  var B:Long = uninitialized
  var C:Long = uninitialized
  var RIP:Int = 0
  var memory:Array[Int] = uninitialized
  var output:List[Int] = Nil
  
  var abortOnJump = false

  def run():Unit = {
    while (RIP >= 0 && RIP < memory.length) {
      execute()
    }
  }

  def execute():Unit = {
    memory(RIP) match
      case 0 => adv()
      case 1 => bxl()
      case 2 => bst()
      case 3 => jnz()
      case 4 => bxc()
      case 5 => out()
      case 6 => bdv()
      case 7 => cdv()
    RIP += 2
  }

  def adv():Unit = {
    A /= (1 << combo)
  }

  def bxl():Unit = {
    B ^= operand
  }

  def bst():Unit = {
    B = combo & 0x7
  }

  def jnz():Unit = {
    if !abortOnJump then 
      if A != 0 then RIP = operand - 2
    else
      RIP = Int.MaxValue-5  
  }

  def bxc():Unit = {
    B ^= C
  }

  def out():Unit = {
    output ::= (combo & 0x7).toInt
  }

  def bdv(): Unit = {
    B = A / (1 << combo)
  }

  def cdv(): Unit = {
    C = A / (1 << combo)
  }

  def combo:Long = combo(operand)

  def combo(x:Int):Long = {
    x match
      case 0|1|2|3 => x
      case 4 => A
      case 5 => B
      case 6 => C
      case _ => throw new IllegalArgumentException()
  }

  private def operand:Int = memory(RIP+1)
}

object Computer {
  def apply(a:Int, b:Int, c:Int, memory:Seq[Int]):Computer = {
    val computer = new Computer
    computer.A = a
    computer.B = b
    computer.C = c
    computer.memory = memory.toArray
    computer
  }
}
