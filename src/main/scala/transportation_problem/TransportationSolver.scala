package transportation_problem

import common.MatrixOld

trait TransportationSolver {
  def solve(costs: MatrixOld[Double]): MatrixOld[Double]
}
