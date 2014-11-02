package tsp_bb

import common.MatrixOld
import hungarian_method.Cost

object Main {
  def main(args: Array[String]) {
    val matrix = new MatrixOld[Double](
      Vector[Vector[Double]](
        Vector(inf, 12, 14, 5, 12),
        Vector(11, inf, 12, 12, 4),
        Vector(12, 5, inf, 13, 12),
        Vector(5, 13, 12, inf, 13),
        Vector(12, 12, 6, 15, inf)
      ))

    val exampleMatrix = new MatrixOld[Double](
      Vector[Vector[Double]](
        Vector(inf,  7,     1,     8,     7),
        Vector(9,  inf,     9,     2,     6),
        Vector(2,   12,   inf,    11,    10),
        Vector(9,    9,    12,   inf,     4),
        Vector(8,    1,    12,    10,   inf)
      )
    )

    val matrixTest = new MatrixOld[Double](
      Vector(
        Vector[Double](  0,   7,   1,  8, 7),
        Vector[Double](  9, 0,   9,    2,   6),
        Vector[Double](  2,  12, 0,   11,  10),
        Vector[Double](  9,   9, 12,  0,   4),
        Vector[Double](  8,   1, 12,  10, 0)
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

    def buildLSequence(mat: MatrixOld[Cost], zeroRow: Int, zeroCol: Int) = {
      def loop(mat: MatrixOld[Cost], currRow: Int, currCol: Int, sequence: List[(Int, Int)]): List[(Int, Int)] =
        if (mat(currRow, currCol).mark2) {
          val newElemRow = mat.col(currCol).indexWhere((c) => c.mark1)
          if (newElemRow != -1)
            loop(mat, newElemRow, currCol, sequence :+ (newElemRow, currCol))
          else
            sequence
        }
        else {
          val newElemCol = mat.row(currRow).indexWhere((c) => c.mark2)
          if (newElemCol != -1)
            loop(mat, currRow, newElemCol, sequence :+ (currRow, newElemCol))
          else
            sequence
        }

      loop(mat, zeroRow, zeroCol, List((zeroRow, zeroCol)))
    }

    val mat = new MatrixOld[Cost] (
      Vector(
        Vector(new Cost(0, true, false), new Cost(7, false, false), new Cost(7, false, false), new Cost(0, false, true)),
        Vector(new Cost(0, false, true), new Cost(0, true, false), new Cost(1, false, false), new Cost(1, false, false)),
        Vector(new Cost(0, false, true), new Cost(3, false, false), new Cost(0, true, false), new Cost(1, false, false)),
        Vector(new Cost(1, false, false), new Cost(0, false, true), new Cost(3, false, false), new Cost(1, false, false))
      )
    )

    println(buildLSequence(mat, 3, 1))
  }
}

