package tsp_bb

import hungarian_method.HungarianSolver

object Main {
  def inf = Double.PositiveInfinity

  def main(args: Array[String]) {
    val matrix: Vector[Vector[Double]] =
      Vector(
        Vector(inf,   7,   1,    8,   7),
        Vector(  9, inf,   9,    2,   6),
        Vector(  2,  12, inf,   11,  10),
        Vector(  9,   9,   12, inf,   4),
        Vector(  8,   1,   12,  10, inf)
      )

    val solver =
      new HungarianSolver(
        matrix,
        (mat, msg, mr, mc) => {}
      )

    val (x, opt) = solver.solve(maximize = false)

    println(x)
    println(opt)
  }
}

