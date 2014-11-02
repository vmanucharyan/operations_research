package transportation_problem

import common.Matrix

trait FeasibleSolutionFinder {
  def apply(costMatrix: Matrix[Double]): Matrix[Double]
}
