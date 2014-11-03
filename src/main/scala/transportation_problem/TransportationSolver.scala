package transportation_problem

import common.Matrix

trait TransportationSolver {
  def solve(problem: TransportPack): Matrix[Double]
}
