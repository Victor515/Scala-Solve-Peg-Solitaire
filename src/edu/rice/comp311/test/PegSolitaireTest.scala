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
    val board = Board(5, Set((2,0)))
    val peg = (0,0)
    assertEquals("Next move for 3*3 board is correct" +
      "hole: (2,0), peg: (0,0)", List(Move((0,0), (2, 0))), nextMove(board, peg))
  }

  def testNextMove2: Unit = {
    val board = Board(5, Set((2,2)))
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


  def testNextRemoval1: Unit = {
    val board = Board(9, Set((1, 1)))
    val coordinates = generateCoordinates(4)
    val segRm = SegmentRemoval((2,2),(2,1),(2,0))
    assertTrue("Next Removal of 4*4 board contains SegmentRemoval((2,2),(2,1),(2,0)), " +
      "peg: (2,2), hole: (1,1)", nextRemoval(board, (2,2), coordinates) contains segRm)

    val move = Move((3,3), (1,1))
    assertTrue("Next Removal of 4*4 board contains Move((3,3),(1,1)), " +
      "peg: (3,3), hole: (1,1)", nextRemoval(board, (3,3), coordinates) contains move)
  }

  def testNextRemoval2: Unit = {
    val board = Board(9, Set((3, 3)))
    val coordinates = generateCoordinates(4)
    val segRm = SegmentRemoval((2,2),(2,1),(2,0))
    assertTrue("Next Removal of 4*4 board contains SegmentRemoval((2,2),(2,1),(2,0)), " +
      "peg: (2,2), hole: (3,3)", nextRemoval(board, (2,2), coordinates) contains segRm)

    val move = Move((1,1), (3,3))
    assertTrue("Next Removal of 4*4 board contains Move((1,1),(3,3)), " +
      "peg: (1,1), hole: (3,3)", nextRemoval(board, (1,1), coordinates) contains move)
  }

  def testReduceBoardWithRemoval: Unit = {
    val board = Board(9, Set((3, 3)))
    val segRm = SegmentRemoval((2,2),(2,1),(2,0))
    assertEquals("Next state of 4*4 board is correct: hole: (3,3), segmentRemoval: ((2,2),(2,1),(2,0))",
      Board(6, Set((3,3),(2,2),(2,1),(2,0))), reduceBoardWithRemoval(board, segRm))

    val move = Move((1,1), (3,3))
    assertEquals("Next state of 4*4 board is correct: hole: (3,3), move: ((1,1),(3,3))",
      Board(8, Set((1,1),(2,2))), reduceBoardWithRemoval(board, move))
  }

  def testBigStepSolve: Unit = {
    assertEquals("Board of size 3 is not solvable", true, solve(3, (2,0)).isEmpty)
  }
}