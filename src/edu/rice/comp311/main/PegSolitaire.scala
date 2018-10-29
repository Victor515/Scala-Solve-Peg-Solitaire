package edu.rice.comp311.main


object PegSolitaire {
  /**
    *
    * @param size size of the board
    * @param hole the initial hole position
    * @return One possible solution to the peg solitaire(list of moves), or None if none is found
    */
  def solve(size: Int, hole: (Int,Int)): Option[List[Move]] = {
    /**
      *
      * @param board current board
      * @param moves list of moves leading to current board state
      * @return one possible solution to the peg solitaire problem, represented by a list of moves
      */
    def inner(board: Board, moves: List[Move]): Option[List[Move]] = {
      board match {
        case Board(_, 1, _) => Option(moves)
        case _ => {
          // TODO: find all possible next moves, generate new boards with them and continue to solve
          inner()
        }
      }

    }

    inner(Board(size, size * (size + 1)/2, Set(hole)), List())
  }

  /**
    *
    * @param board current board
    * @return set of all possible moves
    */
  def nextMoves(board: Board): Option[Set[Move]] = {
    return None
  }

  /**
    *
    * @param board current board
    * @param move next move
    * @return next board state
    */
  def reduceBoard(board: Board, move: Move): Board = ???


}

abstract class Removal

case class Move(from: (Int, Int), to: (Int, Int)) extends Removal {
  override def toString() = from + "->" + to
}

/**
  * @param size the size of board
  * @param pegs number of pegs remained in the board
  * @param holes positions of holes in the board
  */
case class Board(size: Int, pegs: Int, holes: Set[(Int, Int)])