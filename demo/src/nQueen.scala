
object nQueen {
  case class QueenPosition(val row: Int, val column: Int)
  type Solution = List[QueenPosition]
  type Solutions = List[Solution]

  def nQueens(n: Int): Solutions = {

    def place(k: Int): Solutions = {
      if (k == 0) {
        List(Nil)
      } else {
        for {
          queenPositions <- place(k - 1)
          i <- 0 until n
          queenPosition = QueenPosition(k - 1, i)
          if isAllowed(queenPositions, queenPosition)
        } yield queenPosition :: queenPositions
      }
    }

    def isAllowed(queens: Solution, queenPosition: QueenPosition): Boolean = {
      queens.forall{ queen =>
        queen.row != queenPosition.row &&
          queen.column != queenPosition.column &&
          math.abs(queen.row - queenPosition.row) != math.abs(queen.column - queenPosition.column)
      }
    }

    place(n)
  }


  def stringifyQueenPositions(queenPositions: Solution): String = {
    val total = queenPositions.length

    val colVector = queenPositions.sortBy(_.row).foldRight(Vector[Int]()) {
      _.column +: _
    }

    (for (row <- 0 until total)
      yield {
        val qCol = colVector(row)
        (for (column <- 0 until total)
          yield {
            if (qCol == column) "Q" else "*"
          }) mkString " "
      }) mkString "\n"
  }


  def stringifySolutions(solutions: Solutions): String = {
    solutions map stringifyQueenPositions mkString "\n\n"
  }

  def main(args: Array[String]): Unit = {
    println(stringifySolutions(nQueens(5)))

  }
}
