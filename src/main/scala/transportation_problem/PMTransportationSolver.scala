package transportation_problem

import common.Matrix

class PMTransportationSolver(val fsFinder: FeasibleSolutionFinder,
                             val callback: (Route, Matrix[Double], Int) => Unit = (_,_,_) => Unit)
  extends TransportationSolver
{
  case class Equation(ui: Int, vi: Int, ans: Double)
  case class EquationSolution(u: Map[Int, Double], v: Map[Int, Double])

  val cycleBuilder = new TransportCycleBuilder()

  override def solve(problem: TransportPack): Matrix[Double] = {
    val firstBsf = fsFinder.find(problem)
    improveSolution(problem, firstBsf, 1)
  }

  def improveSolution(problem: TransportPack, bsf: Matrix[Double], iter: Int = 0): Matrix[Double] = {
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
      val (modifiedBsf, cycle) = modifyBsf(d, bsf)
      callback(cycle, bsf, iter)
      improveSolution(problem, modifiedBsf, iter + 1)
    }
  }

  def solveEqSystem(equations: IndexedSeq[Equation]): EquationSolution = {
    def iter(known: EquationSolution): EquationSolution = {
      if (known.u.size + known.v.size == equations.length + 1)
        known
      else {
        val canSolveV = equations filter (e => known.u.contains(e.ui))
        val canSolveU = equations filter (e => known.v.contains(e.vi))
        val solvedU = canSolveU map (e => (e.ui, e.ans - known.v(e.vi)))
        val solvedV = canSolveV map (e => (e.vi, e.ans - known.u(e.ui)))

        iter(new EquationSolution(known.u ++ solvedU, known.v ++ solvedV))
      }
    }

    val unknownsCount = equations.map(x => x.ui).toSet.size + equations.map(x => x.vi).toSet.size
    val firstKnowns = (0 until unknownsCount - equations.size).map(i => (equations(i).ui, 0.0)).toMap

    iter(new EquationSolution(firstKnowns, Map()))
  }

  def modifyBsf(d: Iterable[((Int, Int), Double)], bsf: Matrix[Double]): (Matrix[Double], Route) = {
    def allBasisCells: Seq[(Int, Int)] =
      bsf.flatMap(
        condition = (v, ri, ci) => v >= 0,
        transform = (v, ri, ci) => (ri, ci)
      )

    val ((minRow, minCol), minValue) = d minBy (x => x._2)
    val cycle = cycleBuilder.findCycle(allBasisCells, minRow, minCol)

    val cellToExclude = cycle.seq
      .filter(cell => !cell.markPlus)
      .minBy(cell => bsf(cell.row, cell.col))

    val excludeValue = bsf(cellToExclude.row, cellToExclude.col)

    val newMatrix = bsf.mapIndexed(
      (v, r, c) =>
        if (r == cellToExclude.row && c == cellToExclude.col) --
        else if (r == minRow && c == minCol) excludeValue
        else {
          val cycleCell = cycle.seq.find(cell => cell.row == r && cell.col == c)
          cycleCell match {
            case Some(cell) =>
              if (cell.markPlus) bsf(cell.row, cell.col) + excludeValue
              else bsf(cell.row, cell.col) - excludeValue

            case None => bsf(r, c)
          }
        }
    )

    (newMatrix, cycle)
  }

}
