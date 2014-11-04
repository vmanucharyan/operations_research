package transportation_problem

import common.Matrix

class PMTransportationSolver(val fsFinder: FeasibleSolutionFinder) extends TransportationSolver {
  class Equation(val ui: Int, val vi: Int, val ans: Double)
  class EqSolution(val u: Map[Int, Double], val v: Map[Int, Double])

  override def solve(problem: TransportPack): Matrix[Double] = ???

  private def improveSolution(table: TransportPack): TransportPack = ???

  def solveEqSystem(equations: IndexedSeq[Equation])= {
    def iter(known: EqSolution): EqSolution = {
      if (known.u.size + known.v.size == equations.length + 1)
        known
      else {
        val canSolveV = equations.filter(e => known.u.contains(e.ui))
        val canSolveU = equations.filter((e) => known.v.contains(e.vi))
        val solvedU = for (e <- canSolveU) yield (e.ui, e.ans - known.v(e.vi))
        val solvedV = for (e <- canSolveV) yield (e.vi, e.ans - known.u(e.ui))

        iter(new EqSolution(known.u ++ solvedU, known.v ++ solvedV))
      }
    }

    iter(new EqSolution(Map(equations(0).ui -> 0.0), Map()))
  }
}
