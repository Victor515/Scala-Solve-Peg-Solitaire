package edu.rice.comp311.main

import scala.collection.immutable.Queue


object PegSolitaire {
  /**
    *
    * @param size size of the board
    * @param hole the initial hole position
    * @return One possible solution to the peg solitaire(list of moves), or None if none is found
    */
  def solve(size: Int, hole: (Int,Int)): Option[List[Move]] = {
    val coordinates = generateCoordinates(size)

    solveHelper(Board(size * (size + 1) / 2 - 1, Set(hole)), coordinates, List())
  }

  def generateCoordinates(size: Int): List[(Int, Int)] ={
    val coordinates = {
      for(i <- 0 to size - 1; j <- 0 to i)
        yield (i, j)
    }
    coordinates.toList
  }

  private def solveHelper(board: Board, coordinates: List[(Int, Int)], moves: List[Move]): Option[List[Move]] = {
    if(board.peg == 1){
      Some(moves.reverse)
    }
    else {
      runOneMove(board, coordinates, coordinates, moves)
    }
  }

  private def runOneMove(board: Board, remained: List[(Int, Int)], coordinates: List[(Int, Int)], moves: List[Move]): Option[List[Move]] = {
    remained match {
      case Nil => None
      case x :: xs => {
        nextMove(board, x) match {
          case Nil => runOneMove(board, xs, coordinates, moves)
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

  private def tryNextMoves(nextMoves: List[Move], board: Board, coordinates: List[(Int, Int)],
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

  def bigStepSolve(size: Int, hole: (Int, Int)): Option[List[Removal]] = {
    val coordinates = generateCoordinates(size)

    bigStepSolveHelper(Queue(Board(size * (size + 1) / 2 - 1, Set(hole))), coordinates, Queue(List()))
  }

  def bigStepSolveHelper(boards: Queue[Board], coordinates: List[(Int, Int)],
                         removals: Queue[List[Removal]]): Option[List[Removal]] = {
    // TODO:
    // 1. Dequeue a board and its move list
    // 2. Find all possible next moves
    // 3. generate next boards, add to queue
    // 4. If queue is not empty, repeat
    // Do I need to set to record visited boards?
    val (curBoard,boardqTemp) = boards.dequeue
    val (removalList, removalqTemp) = removals.dequeue

    if(curBoard.peg == 1){
      Some(removalList.reverse)
    }else{
      val (nextBoards, nextRemovals) = getNextBoard(curBoard, removalList, coordinates)
      val boardq = boardqTemp.enqueue(nextBoards)
      val removalq = removalqTemp.enqueue(nextRemovals)
      if(boardq.nonEmpty){
        bigStepSolveHelper(boardq, coordinates, removalq) // BFS
      }else{
        None
      }
    }

  }

  /**
    *
    * @param board current board
    * @param removalList the removal list leading to current board state
    * @param coordinates coordinates of board
    * @return list of all possible next boards, and list of all corresponding removal lists
    */
  def getNextBoard(board: Board, removalList: List[Removal],
                   coordinates: List[(Int, Int)]): (List[Board], List[List[Removal]]) = {
    coordinates match {
      case Nil => (Nil, Nil)
      case coord :: others => {
        nextRemoval(board, coord, coordinates) match {
          case Nil => getNextBoard(board, removalList, others)
          case removals => {
            val (boardList1, removalList1) = runOneRemoval(board, removalList, removals)
            val (boardList2, removalList2) = getNextBoard(board, removalList, others)
            (boardList1 ++ boardList2, removalList1 ++ removalList2)
          }
        }
      }
    }
  }

  def runOneRemoval(board: Board, rmList: List[Removal],
                    removals: List[Removal]): (List[Board], List[List[Removal]]) = {
    removals match {
      case Nil => (Nil, Nil)
      case removal :: others => {
        val (boardList: List[Board], removalList: List[List[Removal]]) = runOneRemoval(board, rmList, others)
        (reduceBoardWithRemoval(board, removal) :: boardList, (removal :: rmList) :: removalList)
      }
    }
  }

  def nextRemoval(board: Board, peg: (Int, Int), coordinates: List[(Int, Int)]): List[Removal] = {
    val holes = board.holes
    var ans: List[Removal] = Nil
    if(holes contains peg){
      ans
    }
    ans = nextMove(board, peg)
    val (x, y) = peg

    var pegsRemoval :List[(Int, Int)] = Nil
    if(!(holes contains (x - 2, y - 2)) && !(holes contains (x - 1, y - 1))
      && (coordinates contains (x - 2, y - 2)) && (coordinates contains (x - 1, y - 2))){
      pegsRemoval = (x - 2, y - 2) :: pegsRemoval
    }
    if(!(holes contains (x - 2, y)) && !(holes contains (x - 1, y))
      && (coordinates contains (x - 2, y)) && (coordinates contains (x - 1, y))){
      pegsRemoval = (x - 2, y) :: pegsRemoval
    }
    if(!(holes contains (x, y - 2)) && !(holes contains (x, y - 1))
      && (coordinates contains (x, y - 2)) && (coordinates contains (x, y - 1))){
      pegsRemoval = (x, y - 2) :: pegsRemoval
    }
    if(!(holes contains (x, y + 2)) && !(holes contains (x, y + 1))
      && (coordinates contains (x, y + 2)) && (coordinates contains (x, y + 1))){
      pegsRemoval = (x, y + 2) :: pegsRemoval
    }
    if(!(holes contains (x + 2, y)) && !(holes contains (x + 1, y))
      && (coordinates contains (x + 2, y)) && (coordinates contains (x + 1, y))){
      pegsRemoval = (x + 2, y) :: pegsRemoval
    }
    if(!(holes contains (x + 2, y + 2)) && !(holes contains (x + 1, y + 1))
      && (coordinates contains (x + 2, y + 2)) && (coordinates contains (x + 1, y + 1))){
      pegsRemoval = (x + 2, y + 2) :: pegsRemoval
    }

    var hasAdjacentHoles = false

    if((holes contains (x - 1, y - 1)) && !(holes contains (x + 1, y + 1))
      && (coordinates contains (x + 1, y + 1))){
      hasAdjacentHoles = true
    }
    if((holes contains (x + 1, y + 1)) && !(holes contains (x - 1, y - 1))
      && (coordinates contains (x - 1, y - 1))){
      hasAdjacentHoles = true
    }
    if((holes contains (x - 1, y)) && !(holes contains (x + 1, y))
      && (coordinates contains (x + 1, y))){
      hasAdjacentHoles = true
    }
    if((holes contains (x + 1, y)) && !(holes contains (x - 1, y))
      && (coordinates contains (x - 1, y))){
      hasAdjacentHoles = true
    }
    if((holes contains (x, y - 1)) && !(holes contains (x , y + 1))
      && (coordinates contains (x, y + 1))){
      hasAdjacentHoles = true
    }
    if((holes contains (x, y + 1)) && !(holes contains (x, y - 1))
      && (coordinates contains (x, y - 1))){
      hasAdjacentHoles = true
    }

    if(hasAdjacentHoles){
      for(pegRemoval <- pegsRemoval){
        ans = SegmentRemoval(peg, ((peg._1 + pegRemoval._1)/2, (peg._2 + pegRemoval._2)/2), pegRemoval) :: ans
      }
    }

    ans
  }

  def reduceBoardWithRemoval(board: Board, removal: Removal): Board = {
    removal match {
      case Move(from, to) => reduceBoard(board, Move(from, to))
      case SegmentRemoval(peg1, peg2, peg3) => {
        val holes = board.holes + peg1 + peg2 + peg3
        Board(board.peg - 3, holes)
      }
    }
  }

}

abstract class Removal

case class Move(from: (Int, Int), to: (Int, Int)) extends Removal {
  override def toString() = from + "->" + to
}

case class SegmentRemoval(peg1: (Int, Int), peg2: (Int, Int), peg3: (Int, Int))
  extends Removal


/**
  * @param peg number of pegs in the board
  * @param holes positions of holes in the board
  */
case class Board(peg: Int, holes: Set[(Int, Int)]){
  override def toString() = "Pegs: " + peg + "Holes: " + holes.toString()
}