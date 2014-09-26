package tsp_bb

import hungarian_method.{HungarianSolver, Matrix}
import scala.collection.immutable.Queue

object TravellingSalesmanProblemSolver {
  def solve(costs: Matrix[Double]) = {
    val initialDestinations = buildInitialDestMatrix(costs.rowCount)
    val taskQueue = Queue(costs)

  }

  private def optimalValue(destMat: Matrix[Int], costMat: Matrix[Double]) =
    costMat
      .flat().zip(costMat.flat())
      .filter { case (c, x) => c.isInfinity }
      .map { case (c, x) => c * x }
      .sum

  private def buildInitialDestMatrix(n: Int) =
    destMatrixFromFullCycle(
      for (i <- 0 until n) yield if (i != n - 1) i + 1 else 0)

  private def destMatrixFromFullCycle(seq: IndexedSeq[Int]) =
    new Matrix[Double](
      rowCount = seq.size,
      colCount = seq.size,
      initializer = (ri, ci) => if (seq(ri) == ci) 1 else 0)

  private def completeTask(queue: Queue[Matrix[Double]], optVal: Double, destMat: Matrix[Int])
  : (Queue[Matrix[Double]], Double, Matrix[Int]) = {
    def isFullCycle(destMat: Matrix[Int]) = true

    if (queue.isEmpty)
      (queue, optVal, destMat)
    else {
      val (costs, newQueue) = queue.dequeue
      val hungarianSolver = new HungarianSolver(costs)
      val (newDestMat, newOptVal) = hungarianSolver.solve(maximize = false)

      if (newOptVal > optVal)
        completeTask(newQueue, optVal, destMat)
      else
        if (isFullCycle(newDestMat))
          completeTask(newQueue, newOptVal, newDestMat)
        else
          completeTask(newQueue, newOptVal, newDestMat)
    }
  }

  private def findAllCycles(destMat: Matrix[Int]) = {
    def findSequences(destMat: Matrix[Int]) =
      for (row <- destMat.rows) yield
        row.indexOf((v: Int) => v == 0)
  }
}
