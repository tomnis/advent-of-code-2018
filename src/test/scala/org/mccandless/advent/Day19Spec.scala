package org.mccandless.advent

import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * Created by tdm on 2018-12-27.
  */
class Day19Spec extends FlatSpec with Matchers {

  def loadProgram(filename: String): (Directive, Seq[Op]) = {
    val lines = io.Source.fromResource(filename).getLines.toList
    val directiveStr = lines.head
    require(directiveStr.startsWith("#ip "))
    val directive: Directive = IpDirective(directiveStr.drop(4).toInt)

    val instructions = lines.drop(1).map { line =>
      val components = line.split(" ")
      val nums = components.drop(1).map(_.toInt)
      val args: (Int, Int, Int) = (nums(0), nums(1), nums(2))
      Op.apply(components.head, args)
    }
    (directive, instructions)
  }

  "day 19" should "solve" in {
    val (dir, instructions) = this.loadProgram("day19.txt")
    val m: Machine = Machine(dir, instructions)
    m.run()
    println(s"value in register 0: ${m.registers.head}")

  }
}


case class Machine(directive: Directive, program: Seq[Op]) {
  var registers: Seq[Int] = List(0, 2, 0, 9, 10551347, 10551346) // 1 +: Seq.fill(5)(0)
  def run(): Unit = {
    var ip: Int = 9

    while(0 <= ip && ip < program.length) {

      val registersAfterDirective = this.directive.before(ip, registers)
      val op: Op = program(ip)
      val registersAfterOp = op.eval(registersAfterDirective)
      this.registers = registersAfterOp
      ip = directive.after(registersAfterOp)

      println(s"ip=$ip $registersAfterDirective $op $registersAfterOp")
      Thread.sleep(100)

      ip += 1
    }
  }
}


sealed trait Directive {
  def before(ip: Int, registers: Seq[Int]): Seq[Int]
  def after(registers: Seq[Int]): Int
}
case class IpDirective(register: Int) extends Directive {
  // its value is written to that register just before each instruction is executed, and
  override def before(ip: Int, registers: Seq[Int]): Seq[Int] = {
    registers.zipWithIndex.map { case (r, rind) => if (rind == this.register) ip else r }
  }

  // the value of that register is written back to the instruction pointer immediately after each instruction finishes execution
  override def after(registers: Seq[Int]): Int = {
    registers(this.register)
  }
}
