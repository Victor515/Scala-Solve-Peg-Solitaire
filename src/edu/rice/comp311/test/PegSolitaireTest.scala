package edu.rice.comp311.test;
import edu.rice.comp311.main._
import edu.rice.comp311.main.PegSolitaire._;

import junit.framework.TestCase
import org.junit.Assert._


case class PegSolitaireTest(name: String) extends TestCase(name){


  /**
    *      x
    *     x x   -> find next move for (0,0): Move((0,0),(2,0))
    *    o x x
    */
  def testNextMove1: Unit = {
    val board = Board(6, Set((2,0)))
    val peg = (0,0)
    assertEquals("Next move for 3*3 board is correct" +
      "hole: (2,0), peg: (0,0)", List(Move((0,0), (2, 0))), nextMove(board, peg))
  }

  def testNextMove2: Unit = {
    val board = Board(6, Set((2,2)))
    val peg = (0,0)
    assertEquals("Next move for 3*3 board is correct" +
      "hole: (2,2), peg: (0,0)", List(Move((0,0), (2, 2))), nextMove(board, peg))
  }

  def testReduceBoard: Unit = {
    val board = Board(6, Set((2,2)))
    val move = Move((0,0), (2,2))
    assertEquals("Next state of 3*3 board is correct: " +
      "hole: (2,2), move: (0,0) -> (2,2)", Board(5, Set((0,0),(1,1))), reduceBoard(board, move))
  }

  def testSolve1: Unit = {
    assertEquals("Board of size 3 is not solvable", true, solve(3, (2,0)).isEmpty)
  }

  def testSolve2: Unit = {
    assertEquals("Board of size 5 is solvable", true, solve(5, (2,0)).nonEmpty)
  }

  def testSolve3: Unit = {
    assertEquals("Board of size 7 with one hole at (1,0) is solvable", true, solve(7, (1,0)).nonEmpty)
  }
}