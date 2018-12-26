package org.mccandless.minotaur

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Day17Spec extends FlatSpec with Matchers {

  type Board = mutable.Seq[mutable.Seq[Char]]

  val clay: Char = '#'
  val sand: Char = '.'
  val spring: Char = '+'
  val flowingWater: Char = '|'
  val stillWater: Char = '~'

  def parseRow(clays: Seq[Clay], y: Int, minX: Int, maxX: Int): mutable.Seq[Char] = {
    var row: mutable.Seq[Char] = mutable.ListBuffer.empty
    for {
      x <- minX to maxX
    } {
      val c: Char = if (clays.exists(c => c.x.contains(x) && c.y.contains(y))) clay
      else if (y == 0 && x == 500) spring
      else sand
      row = row :+ c
    }

    row
  }

  def parseSingle(s: String): Int = s.drop(2).toInt
  def parseRange(s: String): Seq[Int] = {
    val sComponents: Seq[String] = s.drop(2).split("\\.\\.")
    require(sComponents.length == 2)
    val start: Int = sComponents.head.toInt
    val end: Int = sComponents.tail.head.toInt
    start to end
  }

  def parseInput(filename: String): Board = {
    val lines: Seq[String] = io.Source.fromResource(filename).getLines.toSeq
    val veins: Seq[Clay] = lines.map { l: String =>
      val components: Seq[String] = l.split(", ").toSeq
      val x: Seq[Int] = if (components.head.startsWith("x")) Seq(parseSingle(components.head)) else parseRange(components.tail.head)
      val y: Seq[Int] = if (components.head.startsWith("y")) Seq(parseSingle(components.head)) else parseRange(components.tail.head)
      Clay(x, y)
    }

    val minX: Int = veins.map(_.x.head).min
    val maxX: Int = veins.map(_.x.head).max
    val minY: Int = veins.map(_.y.head).min
    val maxY: Int = veins.map(_.y.head).max

    var board: Board = mutable.ListBuffer.empty
    for {
      row <- minY - 4 to maxY
    } board = board :+ parseRow(veins, row, minX - 5, maxX + 5)
    board
  }


  def isInBounds(board: Board, row: Int, col: Int): Boolean = {
    0 <= row && row < board.length && 0 <= col && col < board(row).length
  }

  /**
    *
    * @param board
    * @param row
    * @param col
    * @return true iff (row, col) should be changed to still water
    */
  def isInReservoir(board: Board, row: Int, col: Int): Boolean = {
    require(board(row)(col) == sand || board(row)(col) == flowingWater || board(row)(col) == stillWater)
    if (!isInBounds(board, row + 1, col)) {
      false
    }
    else if (board(row)(col) == stillWater) {
      true
    }
    else {
      // check below
//      val below: Char = board(row + 1)(col)
      // below must be clay or still water

      // need to check left
      val left: Boolean = {
        var lCol: Int = col
        while (isInBounds(board, row, lCol) && board(row)(lCol) != clay && (board(row + 1)(lCol) == clay || board(row + 1)(lCol) == stillWater)){
          lCol -= 1
        }
        val cur = board(row)(lCol)
        val down = board(row + 1)(lCol)
        cur == clay && (down == clay || down == stillWater)
      }

      // check right
      val right: Boolean = {
        var rCol: Int = col
        while (isInBounds(board, row, rCol) && board(row)(rCol) != clay && (board(row + 1)(rCol) == clay || board(row + 1)(rCol) == stillWater)) {
          rCol += 1
        }
        val cur = board(row)(rCol)
        val down = board(row + 1)(rCol)
        cur == clay && (down == clay || down == stillWater)
      }

      //
      left && right
    }
  }


  def releaseWater(board: Board): Board = {
    val sources: Seq[(Int, Int)] = for {
      row <- board.indices
      col <- board(row).indices
      if board(row)(col) == spring
    } yield (row, col)


    sources foreach { source =>
      var s: List[(Int, Int)] = (source._1 + 1, source._2) :: Nil
      var discovered: Set[(Int, Int)] = Set.empty
      var oldScore: Int = 0
      while(s.nonEmpty && score(board) != oldScore) {
        val v = s.head
        val (row, col) = v
        s = s.tail
        printState(board)
        Thread.sleep(200)

        if (!discovered.contains(v)) { //} && row.until(board.length).forall(!discovered.contains(_, col))) {
          discovered += v
          oldScore = score(board)
          if (board(row)(col) != spring) { // } !discovered.contains(row + 1, col) &&)) {
//            if (row < board.length - 1 && (board(row + 1)(col) == clay || board(row + 1)(col) == stillWater)) // && isInReservoir(board, row, col))
//              board(row)(col) = stillWater
//            else {
              board(row)(col) = flowingWater
//            }
          }
          s = v :: s
          // add outgoing edges
          val left = (row, col - 1)
          val right = (row, col + 1)
          val down = (row + 1, col)

          if (isInBounds(board, right._1, right._2) && board(right._1)(right._2) == sand && right._1 < board.length -1) {
              s = right :: s
          }
          else {
            discovered += right
          }
          if (isInBounds(board, left._1, left._2) && board(left._1)(left._2) == sand && left._1 < board.length - 1) {
              s = left :: s
          }
          else {
            discovered += left
          }
          if (isInBounds(board, down._1, down._2) && board(down._1)(down._2) == sand && down._1 < board.length) {
            s = down :: s
          }
          else {
            discovered += down
          }
        }
        // we have visited this node before, so just check to see if we need to update its flowing state
        else {
          println(s"already visited ($row, $col)")

          if (isInReservoir(board, row, col) && board(row)(col) == flowingWater) {
            board(row)(col) = stillWater
          }
        }
      }
    }
    board
  }

  /**
    *
    * @param board
    * @param yMin
    * @param yMax
    * @return number of squares currently reached by water
    */
  def score(board: Board): Int = {
    val rowScores: Seq[Int] = for {
      row <- board.indices
    } yield board(row).count(p => p == spring || p == flowingWater || p == stillWater)
    rowScores.sum
  }

  def printState(board: Board): Unit = {
    for {
      row <- board.indices
      col <- board(row).indices
    } {
      print(board(row)(col))
      if (col == board(row).length - 1) {
        println(s"  $row")
      }
    }
    println()
    println()
  }


  "day 17" should "solve" in {
    var board: Board = this.parseInput("day17_small.txt")


    board = releaseWater(board)
    //    print(score(board))
    //    printState(board)
  }
}


case class Clay(x: Seq[Int], y: Seq[Int]) {
  require(x.length == 1 || y.length == 1)
}
