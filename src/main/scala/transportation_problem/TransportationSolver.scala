package transportation_problem

import common.Matrix

trait TransportationSolver {
  def solve(costs: Matrix[Double]): Matrix[Double]
}
