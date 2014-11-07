import common.Matrix

package object transportation_problem {
  val -- = Double.NegativeInfinity
  def isNonExistent(v: Double): Boolean = v.isNegInfinity

  def printMatrix(matrix: Matrix[Double]) = {
    for (row <- matrix.rows) {
      for (e <- row) {
        if (!isNonExistent(e)) System.out.print(f"$e%1.0f\t")
        else System.out.print(f"--\t")
      }
      System.out.println()
    }
  }

  def printCycle(cycle: Route): Unit = {
    for (i <- 0 until cycle.seq.size - 1)
      System.out.print(f"(${cycle.seq(i).row}, ${cycle.seq(i).col}) -> ")
    System.out.println(f"(${cycle.seq.last.row}, ${cycle.seq.last.col})")
  }
}
