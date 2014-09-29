package tsp_bb

import common.Matrix
import hungarian_method.HungarianSolver

object Main {
  def main(args: Array[String]) {
    val matrix = new Matrix[Double](
      Vector[Vector[Double]](
        Vector(inf, 12, 14, 5, 12),
        Vector(11, inf, 12, 12, 4),
        Vector(12, 5, inf, 13, 12),
        Vector(5, 13, 12, inf, 13),
        Vector(12, 12, 6, 15, inf)
      ))

    val exampleMatrix = new Matrix[Double](
      Vector[Vector[Double]](
        Vector(inf,  7,     1,     8,     7),
        Vector(9,  inf,     9,     2,     6),
        Vector(2,   12,   inf,    11,    10),
        Vector(9,    9,    12,   inf,     4),
        Vector(8,    1,    12,    10,   inf)
      )
    )

    val matrixTest = new Matrix[Double](
      Vector(
        Vector[Double](0,   7,   1,    8,   7),
        Vector[Double](  9, 0,   9,    2,   6),
        Vector[Double](  2,  12, 0,   11,  10),
        Vector[Double](  9,   9,   12, 0,   4),
        Vector[Double](  8,   1,   12,  10, 0)
      )
    )

    val tspSolver = new TspSolverWithBnb(
      (tr) => {
        println(s"Итерация #${tr.iterationNum}:")
        println(s"Текущая матрица стоимостей:\n${tr.costMat}")
        println(s"Полученная матрица назначений:\n${tr.destMat}")
        println(s"f*: ${tr.fs}")
        println(s"f0: ${tr.f0}")
        println("================================================================")
        println()
      }
    )
    val (opt, dmat) = tspSolver.solve(matrix)

    println(opt)
    println(dmat)
  }
}

