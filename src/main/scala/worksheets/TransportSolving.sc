package worksheets

import common.IndexedSeqExt._

object TransportSolving {
  class Equation(val ui: Int, val vi: Int, val ans: Double)
  class EqSolution(val u: Map[Int, Double], val v: Map[Int, Double])

  def solveEqSystem(equations: IndexedSeq[Equation])= {
    def iter(known: EqSolution): EqSolution = {
      if (known.u.size + known.v.size == equations.length)
        known
      else {
        val canSolveV = equations.filter(e => known.u.contains(e.ui))
        val canSolveU = equations.filter((e) => known.v.contains(e.vi))
        val solvedU = for (e <- canSolveU) yield (e.ui, e.ans - known.v(e.vi))
        val solvedV = for (e <- canSolveV) yield (e.vi, e.ans - known.u(e.ui))

        iter(new EqSolution(known.u ++ solvedU, known.v ++ solvedV))
      }
    }

    iter(new EqSolution(Map(0 -> 0.0), Map()))
  }

  val system = IndexedSeq(
    new Equation(1, 1, 3),
    new Equation(1, 2, 3),
    new Equation(2, 2, 6),
    new Equation(3, 2, 2),
    new Equation(3, 3, 3),
    new Equation(3, 4, 4)
  )

  solveEqSystem(system)
}
