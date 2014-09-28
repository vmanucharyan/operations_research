package tsp_bb

import common.Matrix
import hungarian_method.HungarianSolver
import scala.collection.immutable.Queue

class TspSolverWithBnb {
  def solve(costs: Matrix[Double]) = {
    val initialDestinations = buildInitialDestMatrix(costs.rowCount)
    val taskQueue = Queue(costs)
    val (_, optVal, destMat) = completeTask(taskQueue, optimalValue(initialDestinations, costs), initialDestinations)

    (optVal, destMat)
  }

  private def optimalValue(destMat: Matrix[Int], costMat: Matrix[Double]) =
    costMat
      .flat().zip(destMat.flat())
      .filter { case (c, x) => !c.isInfinity }
      .map { case (c, x) => c * x }
      .sum

  private def buildInitialDestMatrix(n: Int) =
    destMatrixFromFullCycle(
      for (i <- 0 until n) yield if (i != n - 1) i + 1 else 0)

  private def destMatrixFromFullCycle(seq: IndexedSeq[Int]) =
    new Matrix[Int](
      rowCount = seq.size,
      colCount = seq.size,
      initializer = (ri, ci) => if (seq(ri) == ci) 1 else 0)

  private def completeTask(queue: Queue[Matrix[Double]], optVal: Double, destMat: Matrix[Int])
  : (Queue[Matrix[Double]], Double, Matrix[Int]) =
  {
    if (queue.isEmpty)
      (queue, optVal, destMat)
    else {
      val (costs, newQueue) = queue.dequeue
      val hungarianSolver = new HungarianSolver(costs)
      val (newDestMat, newOptVal) = hungarianSolver.solve(maximize = false)
      val cycles = findAllCycles(newDestMat)

      if (newOptVal > optVal)
        completeTask(newQueue, optVal, destMat)
      else
      if (isFullCycle(newDestMat, cycles))
        completeTask(newQueue, newOptVal, newDestMat)
      else
        completeTask(addSubTasks(newQueue, cycles, costs), optVal, destMat)
    }
  }

  private def findAllCycles(destMat: Matrix[Int]) = {
    def transitionsFromMatrix(destMat: Matrix[Int]) =
      for (row <- 0 until destMat.rowCount) yield
        (row, destMat.rows(row).indexOf(1))

    def recurseFindCycle(transitions: List[(Int,Int)], foundCycles: List[List[(Int, Int)]])
    : (List[(Int, Int)], List[List[(Int, Int)]]) =
    {
      def recurseFindPair(transitions: List[(Int, Int)], cycle: List[(Int, Int)])
      : (List[(Int, Int)], List[(Int, Int)]) =
      {
        if (cycle.head._1 == cycle.last._2 || transitions.isEmpty)
          (transitions, cycle)
        else
          recurseFindPair(transitions, cycle ++ transitions.find((tr) => tr._1 == cycle.last._2))
      }

      if (transitions.isEmpty) {
        (transitions, foundCycles)
      }
      else {
        val (_, cycle) = recurseFindPair(transitions.tail, List(transitions.head))
        recurseFindCycle(transitions.tail.filter(p => !cycle.contains(p)), foundCycles :+ cycle)
      }
    }

    val allTransitions = transitionsFromMatrix(destMat).toList
    val (_, foundCycles) = recurseFindCycle(allTransitions, List.empty[List[(Int, Int)]])

    foundCycles
  }

  private def isFullCycle(destMat: Matrix[Int], cycle: List[List[(Int, Int)]]): Boolean =
    cycle.length == 1 && cycle(0).length == destMat.rowCount

  private def addSubTasks(queue: Queue[Matrix[Double]], cycles: List[List[(Int, Int)]], costMatrix: Matrix[Double]) = {
    def createSubTask(queue: Queue[Matrix[Double]], cycle: List[(Int,Int)], task: Matrix[Double])
    : (Queue[Matrix[Double]], List[(Int,Int)]) =
    {
      if (cycle.isEmpty)
        (queue, cycle)
      else {
        val (c1, c2) = cycle.head
        val newMatrix = task.map((v, ri, ci) => if (ri == c1 && ci == c2) inf else v)
        createSubTask(queue.enqueue(newMatrix), cycle.tail, task)
      }
    }

    val minCycle = cycles.minBy((cycle) => cycle.length)
    val (newQueue, _) = createSubTask(queue, minCycle, costMatrix)

    newQueue
  }
}
