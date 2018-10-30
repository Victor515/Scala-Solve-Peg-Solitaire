package edu.rice.comp311.main


object PegSolitaire {
  /**
    *
    * @param size size of the board
    * @param hole the initial hole position
    * @return One possible solution to the peg solitaire(list of moves), or None if none is found
    */
  def solve(size: Int, hole: (Int,Int)): Option[List[Move]] = {
    val coordinates = {
      for(i <- 0 to size - 1; j <- 0 to i)
        yield (i, j)
    }
//    println(coordinates)

    solveHelper(Board(size * (size + 1) / 2 - 1, Set(hole)), coordinates.toList, List())
  }

  def solveHelper(board: Board, coordinates: List[(Int, Int)], moves: List[Move]): Option[List[Move]] = {
//    println(board)
    if(board.peg == 1){
      Some(moves.reverse)
    }
    else {
//      coordinates match {
//        case Nil => None
//        case x :: xs => {
//          nextMove(board, x) match {
//            case Nil => tryOtherCoordinates(board, xs, coordinates, moves)
//            case move :: otherMoves => {
//              solveHelper(reduceBoard(board, move), coordinates, move :: moves) match {
//                case None => tryOtherMoves(otherMoves, board, coordinates, moves)
//                case Some(m) => Some(m)
//              }
//            }
//          }
//        }
//      }
      runOneMove(board, coordinates, coordinates, moves)
    }
  }

  def runOneMove(board: Board, remained: List[(Int, Int)], coordinates: List[(Int, Int)], moves: List[Move]): Option[List[Move]] = {
    remained match {
      case Nil => None
      case x :: xs => {
        nextMove(board, x) match {
          case Nil => runOneMove(board, xs, coordinates, moves)
//          case move :: otherMoves => {
//            solveHelper(reduceBoard(board, move), coordinates, move :: moves) match {
//              case None => tryOtherMoves(otherMoves, board, coordinates, moves)
//              case Some(m) => Some(m)
//            }
//          }
          case nextMoves: List[Move] => {
            tryNextMoves(nextMoves, board, coordinates, moves) match {
              case None => runOneMove(board, xs, coordinates, moves)
              case Some(m) => Some(m)
            }
          }
        }
      }
    }
  }

  def tryNextMoves(nextMoves: List[Move], board: Board, coordinates: List[(Int, Int)],
                   moves: List[Move]): Option[List[Move]] = {
    nextMoves match {
      case Nil => None
      case move :: others => {
        solveHelper(reduceBoard(board, move), coordinates, move :: moves) match {
          case None => tryNextMoves(others, board, coordinates, moves)
          case Some(m) => Some(m)
        }
      }
    }
  }


  /**
    *
    * @param board current board state
    * @param peg the given peg position
    * @return possible moves for the given peg
    */
  def nextMove(board: Board, peg: (Int, Int)): List[Move] = {
    var ans: List[Move] = Nil
    val holes = board.holes

    if(holes contains peg){
      ans
    }
    else {
      if ((holes contains(peg._1, peg._2 - 2)) && !(holes contains(peg._1, peg._2 - 1))) {
        ans = Move(peg, (peg._1, peg._2 - 2)) :: ans
      }
      if ((holes contains(peg._1, peg._2 + 2)) && !(holes contains(peg._1, peg._2 + 1))) {
        ans = Move(peg, (peg._1, peg._2 + 2)) :: ans
      }
      if ((holes contains(peg._1 - 2, peg._2 - 2)) && !(holes contains(peg._1 - 1, peg._2 - 1))) {
        ans = Move(peg, (peg._1 - 2, peg._2 - 2)) :: ans
      }
      if ((holes contains(peg._1 - 2, peg._2)) && !(holes contains(peg._1 - 1, peg._2))) {
        ans = Move(peg, (peg._1 - 2, peg._2)) :: ans
      }
      if ((holes contains(peg._1 + 2, peg._2)) && !(holes contains(peg._1 + 1, peg._2))) {
        ans = Move(peg, (peg._1 + 2, peg._2)) :: ans
      }
      if ((holes contains(peg._1 + 2, peg._2 + 2)) && !(holes contains(peg._1 + 1, peg._2 + 1))) {
        ans = Move(peg, (peg._1 + 2, peg._2 + 2)) :: ans
      }
      ans
    }
  }

  /**
    *
    * @param board current board
    * @param move next move
    * @return next board state
    */
  def reduceBoard(board: Board, move: Move): Board = {
    val toDelete = move.to
    val toAdd1 = move.from
    val toAdd2 = ((move.from._1 + move.to._1)/2, (move.from._2 + move.to._2)/2)
    val holes = (board.holes + toAdd1 + toAdd2 - toDelete)
    Board(board.peg - 1, holes)
  }


}

abstract class Removal

case class Move(from: (Int, Int), to: (Int, Int)) extends Removal {
  override def toString() = from + "->" + to
}

/**
  * @param peg list of coordinates in the board
  * @param holes positions of holes in the board
  */
case class Board(peg: Int, holes: Set[(Int, Int)]){
  override def toString() = "Pegs: " + peg + "Holes: " + holes.toString()
}