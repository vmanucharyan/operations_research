package transportation_problem

import common.Matrix

trait FeasibleSolutionFinder {
  def find(problem: TransportPack): Matrix[Double]
}
