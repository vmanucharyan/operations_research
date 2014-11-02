package transportation_problem

import common.Matrix2

trait FeasibleSolutionFinder {
  def apply(costMatrix: Matrix2[Double]): Matrix2[Double]
}
