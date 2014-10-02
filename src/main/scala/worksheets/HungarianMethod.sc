import common.Matrix
import hungarian_method.{Cost, HungarianSolver}
object HungarianMethod {
  val matrix: Vector[Vector[Double]] =
    Vector(
      Vector(9, 9, 9, 9, 9),
      Vector(9, 8, 1, 8, 8),
      Vector(9, 5, 7, 4, 9),
      Vector(9, 6, 8, 9, 3),
      Vector(9, 3, 9, 4, 3)
    )
  val matrix7: Vector[Vector[Double]] =
    Vector(
      Vector(11, 4, 11, 6, 11),
      Vector(7, 5, 6, 7, 12),
      Vector(9, 7, 8, 10, 10),
      Vector(9, 11, 6, 10, 9),
      Vector(7, 10, 4, 8, 8)
    )
  val example: Vector[Vector[Double]] =
    Vector(
      Vector(5, 6, 7, 1),
      Vector(10, 4, 6, 7),
      Vector(8, 5, 3, 5),
      Vector(12, 5, 9, 8)
    )
  val example2: Vector[Vector[Double]] =
    Vector(
      Vector(1, 0, 1, 4),
      Vector(0, 2, 1, 1),
      Vector(2, 2, 5, 0),
      Vector(3, 1, 0, 5)
    )
  val solver =
    new HungarianSolver(
      matrix,
      (mat, msg, mr, mc) => {}
    )
  val (res, opt) = solver.solve(maximize = true)






  def buildLSequence(mat: Matrix[Cost], zeroRow: Int, zeroCol: Int) = {
    def loop(mat: Matrix[Cost], currRow: Int, currCol: Int, sequence: List[(Int, Int)]): List[(Int, Int)] = {
      val (newElemCol, newElemRow) =
        if (mat(currRow, currCol).mark2)
          (currRow, mat.col(currCol).indexWhere((c) => c.mark1))
        else
          (mat.row(currRow).indexWhere((c) => c.mark2), currCol)

      if (newElemCol == -1 || newElemRow == -1)
        sequence
      else
        loop(mat, newElemRow, newElemCol, sequence :+ (newElemRow, newElemCol))
    }


    loop(mat, zeroRow, zeroCol, List((zeroRow, zeroCol)))
  }
  val mat = new Matrix[Cost] (
    Vector(
      Vector(new Cost(0, true, false), new Cost(7, false, false), new Cost(7, false, false), new Cost(0, false, true)),
      Vector(new Cost(0, false, true), new Cost(0, true, false), new Cost(1, false, false), new Cost(1, false, false)),
      Vector(new Cost(0, false, true), new Cost(3, false, false), new Cost(0, true, false), new Cost(1, false, false)),
      Vector(new Cost(1, false, false), new Cost(0, false, true), new Cost(3, false, false), new Cost(1, false, false))
    )
  )
  buildLSequence(mat, 3, 1)
}
