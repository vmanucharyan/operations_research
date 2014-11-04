package transportation_problem

import common.Matrix

class PMTransportationSolver(val fsFinder: FeasibleSolutionFinder) extends TransportationSolver {
  class Equation(val ui: Int, val vi: Int, val ans: Double)
  class EqSolution(val u: Map[Int, Double], val v: Map[Int, Double])

  override def solve(problem: TransportPack): Matrix[Double] = {
    val firstBsf = fsFinder.find(problem)
    val solution = improveSolution(problem, firstBsf)

    ???
  }

  def improveSolution(problem: TransportPack, bsf: Matrix[Double]): Matrix[Double] = {
    val eqSystem = bsf.flatMap(
      condition = (v, _, _) => v >= 0,
      transform = (v, ri, ci) => new Equation(ri, ci, problem.costs(ri, ci))
    )

    val systemSolution = solveEqSystem(eqSystem)

    val d = bsf.flatMap(
      condition = (v, _, _) => isNonExistent(v),
      transform = (v, ri, ci) =>
        ((ri, ci), problem.costs(ri, ci) - systemSolution.u(ri) - systemSolution.v(ci))
    )

    if (!(d exists (e => e._2 < 0))) bsf
    else {
      val modifiedBsf = modifyBsf(d, bsf)
      improveSolution(problem, modifiedBsf)
    }
  }

  def solveEqSystem(equations: IndexedSeq[Equation]): EqSolution = {
    def iter(known: EqSolution): EqSolution = {
      if (known.u.size + known.v.size == equations.length + 1)
        known
      else {
        val canSolveV = equations filter (e => known.u.contains(e.ui))
        val canSolveU = equations filter (e => known.v.contains(e.vi))

        val solvedU = canSolveU map (e => (e.ui, e.ans - known.v(e.vi)))
        val solvedV = canSolveV map (e => (e.vi, e.ans - known.u(e.ui)))

        iter(new EqSolution(known.u ++ solvedU, known.v ++ solvedV))
      }
    }

    val unknownsCount = equations.map(x => x.ui).toSet.size + equations.map(x => x.vi).toSet.size
    val firstKnowns =  (0 until unknownsCount - equations.size).map(i => (equations(i).ui, 0.0)).toMap

    iter(new EqSolution(firstKnowns, Map()))
  }

  def modifyBsf(d: IndexedSeq[((Int, Int), Double)], bsf: Matrix[Double]): Matrix[Double] = {
    case class CycleElement(value: Double, row: Int, col: Int, plusMark: Boolean, minusMark: Boolean)

    def buildCycle(row: Int, col: Int, cycle: List[CycleElement]): List[CycleElement] = {
      ???
    }

    val ((minRow, minCol), minValue) = d minBy (x => x._2)

    ???
  }
}
