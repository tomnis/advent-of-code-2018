package org.mccandless.advent

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

// TODO ugly code
class Day17Spec extends FlatSpec with Matchers {

  type Board = mutable.Seq[mutable.Seq[Char]]

  val clay: Char = '#'
  val sand: Char = '.'
  val spring: Char = '+'
  val flowingWater: Char = '|'
  val stillWater: Char = '~'
  val solidSquares: Set[Char] = Set(clay, stillWater)
  val permeableSquares: Set[Char] = Set(sand, flowingWater)

  def parseRow(clays: Set[Clay], y: Int, minX: Int, maxX: Int): mutable.Seq[Char] = {
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
    val veins: Set[Clay] = lines.map { l: String =>
      val components: Seq[String] = l.split(", ").toSeq
      val x: Seq[Int] = if (components.head.startsWith("x")) Seq(parseSingle(components.head)) else parseRange(components.tail.head)
      val y: Seq[Int] = if (components.head.startsWith("y")) Seq(parseSingle(components.head)) else parseRange(components.tail.head)
      Clay(x, y)
    }.toSet

    val minX: Int = veins.map(_.x.min).min
    val maxX: Int = veins.map(_.x.max).max
    val minY: Int = 0 // veins.map(_.y.min).min
    val maxY: Int = veins.map(_.y.max).max

    println(s"min y in input: ${veins.map(_.y.min).min}")
    var board: Board = new mutable.ArrayBuffer[mutable.Seq[Char]](maxY)

    for {
      row <- minY to maxY
    } board = board :+ parseRow(veins, row, minX - 10, maxX + 10)
    board
  }


  def isInBounds(board: Board, square: Square): Boolean = isInBounds(board, square.row, square.col)
  def isInBounds(board: Board, row: Int, col: Int): Boolean = {
    0 <= row && row < board.length && 0 <= col && col < board(row).length
  }

  def releaseWater(board: Board): Board = {
    val sources: Seq[(Int, Int)] = for {
      row <- board.indices
      col <- board(row).indices
      if board(row)(col) == spring
    } yield (row, col)


    val queue: mutable.Set[Square] = mutable.Set.empty
    queue += Square(sources.head._1, sources.head._2)
    // initiate a source
    // maintain priority queue of sources (order by -y)
    // loop until queue is empty
    //   if cell down is sand
    //     fill downward until not sand
    //     add original source back to queue
    //   elif next cell down is clay or still water
    //     look left and right for "end" of the row
    //     if both ends are solid, fill intermediate cells with still water
    //       otherwise fill intermediate cells with flowing water
    //     free ends add to queue as sources
    //   elif next cell down is flowing water, or at bottom
    //     mark cell as flowing water
    while (queue.nonEmpty) {
      val current = queue.maxBy(_.row)
      queue -= current

      if (isInBounds(board, current.row + 1, current.col) && board(current.row + 1)(current.col) == sand) {
        // fill downward until not sand
        var (row, col) = (current.row + 1, current.col)
        while (isInBounds(board, row, col) && permeableSquares.contains(board(row)(col))) {
          board(row)(col) = flowingWater
          queue += Square(row, col)
          row += 1
        }
        if (row != current.row + 1 && row < board.length) {
          queue += current
        }
      }
      else if (isInBounds(board, current.row + 1, current.col) && (board(current.row + 1)(current.col) == clay || board(current.row + 1)(current.col) == stillWater)) {
        // find left and right ends of the row
        var left: Int = current.col
        while (isInBounds(board, current.row, left) && (board(current.row + 1)(left) == stillWater || board(current.row + 1)(left) == clay) && board(current.row)(left) != clay) {
          left -= 1
        }
        var right: Int = current.col
        while (isInBounds(board, current.row, right) && (board(current.row + 1)(right) == stillWater || board(current.row + 1)(right) == clay) && board(current.row)(right) != clay) {
          right += 1
        }
        if (solidSquares.contains(board(current.row)(left)) && solidSquares.contains(board(current.row)(right))) {
          for (i <- left + 1 until right) {
            board(current.row)(i) = stillWater
          }
        }
        else {
          val leftBound = if (board(current.row)(left) == sand) left else left + 1
          val rightBound = if (board(current.row)(right) == sand) right else right - 1
          println(s"$leftBound, $rightBound")
          if (board(current.row)(leftBound) == sand && current.row < board.length - 1 && leftBound != current.col) {
            queue += Square(current.row, leftBound)
          }
          if (board(current.row)(rightBound) == sand && current.row < board.length - 1 && rightBound != current.col) {
            queue += Square(current.row, rightBound)
          }
          for (i <- leftBound to rightBound) {
            board(current.row)(i) = flowingWater
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
    } yield board(row).count(p => p == stillWater)
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
    var board: Board = this.parseInput("day17.txt")

    board = releaseWater(board)

    printState(board)
    println(score(board))
  }
}

case class Square(row: Int, col: Int)

case class Clay(x: Seq[Int], y: Seq[Int]) {
  require(x.length == 1 || y.length == 1)
}
