package org.mccandless.advent

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

import Day16Spec._


class Day16Spec extends FlatSpec with Matchers {


  def loadSamples(filename: String): Seq[Sample] = {
    io.Source.fromResource(filename).getLines().toList.grouped(4).map { lines =>
      val before: Seq[Int] = lines(0).drop(9).dropRight(1).split(", ").map(_.toInt)
      val instruction: Seq[Int] = lines(1).split(" ").map(_.toInt)
      val after: Seq[Int] = lines(2).drop(9).dropRight(1).split(", ").map(_.toInt)

      Sample(before, instruction, after)
    }.toList
  }



  def loadProgram(filename: String): Seq[Op] = {
    io.Source.fromResource(filename).getLines.toList.map { line =>
      val instruction: Seq[Int] = line.split(" ").map(_.toInt)
      Op.apply(instruction)
    }
  }

//  val knownOpcodes: mutable.Map()


  "day 16" should "solve" in {
    val samples = this.loadSamples("day16_part1.txt")

    println(s"analyzing ${samples.length} samples")
    println(samples.count(_.numberOpcodesBehavesLike >= 3))
    knownOpcodes.size should be(16)


    val program: Seq[Op] = this.loadProgram("day16_part2.txt")
    val result: Seq[Int] = program.foldLeft(Seq(0, 0, 0, 0)) { (regs, prog) =>
      println(s"${regs.mkString(" ")}        $prog")
      prog.eval(regs)
    }
    println(result.head)
  }
}


object Day16Spec {

  val knownOpcodes: mutable.Map[Int, String] = mutable.Map.empty
}


case class Sample(before: Seq[Int], instruction: Seq[Int], after: Seq[Int]) {

  /** @return number of opcodes that this could behave like */
  def numberOpcodesBehavesLike: Int = {
    val r: Seq[Op] = Seq(
      addr(instruction(1), instruction(2), instruction(3)),
      addi(instruction(1), instruction(2), instruction(3)),
      mulr(instruction(1), instruction(2), instruction(3)),
      muli(instruction(1), instruction(2), instruction(3)),
      banr(instruction(1), instruction(2), instruction(3)),
      bani(instruction(1), instruction(2), instruction(3)),
      borr(instruction(1), instruction(2), instruction(3)),
      bori(instruction(1), instruction(2), instruction(3)),
      setr(instruction(1), instruction(2), instruction(3)),
      seti(instruction(1), instruction(2), instruction(3)),
      gtir(instruction(1), instruction(2), instruction(3)),
      gtri(instruction(1), instruction(2), instruction(3)),
      gtrr(instruction(1), instruction(2), instruction(3)),
      eqir(instruction(1), instruction(2), instruction(3)),
      eqri(instruction(1), instruction(2), instruction(3)),
      eqrr(instruction(1), instruction(2), instruction(3))
    ).filter(checkIfBehavesLike)

    val compatibleUnknown = r.filterNot(p => knownOpcodes.values.toSet.contains(p.name))
    if (compatibleUnknown.size == 1) {
      println(s"${instruction.head} == ${compatibleUnknown.head.name}")
      knownOpcodes += (instruction.head -> compatibleUnknown.head.name)
    }

    r.size
  }

  def checkIfBehavesLike(op: Op): Boolean = {
    op.eval(before) == after
  }
}

sealed trait Op {

  val name: String = this.toString.take(4)
  val inputA: Int
  val inputB: Int
  val outputC: Int

  def eval(registers: Seq[Int]): Seq[Int] = {
    val computedResult: Int = this.result(registers)
    registers.zipWithIndex.map { case (r, rind) => if (rind == outputC) computedResult else r }
  }

  def result(registers: Seq[Int]): Int

  def op(a: Int, b: Int): Int
}


object Op {
  def apply(instruction: Seq[Int]): Op = {
    val c: Class[_] = Class.forName(s"org.mccandless.advent.${knownOpcodes(instruction.head)}")
    c.getConstructor(classOf[Int], classOf[Int], classOf[Int]).newInstance(new java.lang.Integer(instruction(1)), new java.lang.Integer(instruction(2)), new java.lang.Integer(instruction(3))).asInstanceOf[Op]
  }

  // for day 19
  def apply(instruction: String, args: Tuple3[Int, Int, Int]): Op = {
    val c: Class[_] = Class.forName(s"org.mccandless.advent.$instruction")
    c.getConstructor(classOf[Int], classOf[Int], classOf[Int]).newInstance(new java.lang.Integer(args._1), new java.lang.Integer(args._2), new java.lang.Integer(args._3)).asInstanceOf[Op]
  }
}

sealed trait RegisterOp extends Op {
  override def result(registers: Seq[Int]): Int = op(registers(this.inputA), registers(this.inputB))
}

sealed trait ImmediateOp extends Op {
  override def result(registers: Seq[Int]): Int = op(registers(this.inputA), this.inputB)
}

sealed trait AddOp extends Op {
  override def op(a: Int, b: Int): Int = a + b
}
sealed trait MultOp extends Op {
  override def op(a: Int, b: Int): Int = a * b
}

sealed trait AndOp extends Op {
  override def op(a: Int, b: Int): Int = a & b
}
sealed trait OrOp extends Op {
  override def op(a: Int, b: Int): Int = a | b
}

sealed trait AssOp extends Op {
  // b is ignored
  override def op(a: Int, b: Int): Int = a
}

sealed trait GtOp extends Op {
  override def op(a: Int, b: Int): Int = if (a > b) 1 else 0
}


sealed trait EqOp extends Op {
  override def op(a: Int, b: Int): Int = if (a == b) 1 else 0
}




// add register and immediate
case class addr(inputA: Int, inputB: Int, outputC: Int) extends RegisterOp with AddOp
case class addi(inputA: Int, inputB: Int, outputC: Int) extends ImmediateOp with AddOp

// mult register and immediate
case class mulr(inputA: Int, inputB: Int, outputC: Int) extends RegisterOp with MultOp
case class muli(inputA: Int, inputB: Int, outputC: Int) extends ImmediateOp with MultOp

// bitwise and
case class banr(inputA: Int, inputB: Int, outputC: Int) extends RegisterOp with AndOp
case class bani(inputA: Int, inputB: Int, outputC: Int) extends ImmediateOp with AndOp

// bitwise or
case class borr(inputA: Int, inputB: Int, outputC: Int) extends RegisterOp with OrOp
case class bori(inputA: Int, inputB: Int, outputC: Int) extends ImmediateOp with OrOp

// assignment (input B is ignored
case class setr(inputA: Int, inputB: Int, outputC: Int) extends AssOp {
  override def result(registers: Seq[Int]): Int = op(registers(this.inputA), -1)
}
case class seti(inputA: Int, inputB: Int, outputC: Int) extends AssOp {
  override def result(registers: Seq[Int]): Int = op(this.inputA, -1)
}


// greater than testing
case class gtir(inputA: Int, inputB: Int, outputC: Int) extends GtOp {
  override def result(registers: Seq[Int]): Int = op(inputA, registers(inputB))
}
case class gtri(inputA: Int, inputB: Int, outputC: Int) extends GtOp {
  override def result(registers: Seq[Int]): Int = op(registers(inputA), inputB)
}
case class gtrr(inputA: Int, inputB: Int, outputC: Int) extends GtOp {
  override def result(registers: Seq[Int]): Int = op(registers(inputA), registers(inputB))
}

// eq testing
case class eqir(inputA: Int, inputB: Int, outputC: Int) extends EqOp {
  override def result(registers: Seq[Int]): Int = op(inputA, registers(inputB))
}
case class eqri(inputA: Int, inputB: Int, outputC: Int) extends EqOp {
  override def result(registers: Seq[Int]): Int = op(registers(inputA), inputB)
}
case class eqrr(inputA: Int, inputB: Int, outputC: Int) extends EqOp {
  override def result(registers: Seq[Int]): Int = op(registers(inputA), registers(inputB))
}
