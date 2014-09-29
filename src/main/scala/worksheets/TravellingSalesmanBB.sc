import common.Matrix
import hungarian_method.HungarianSolver
import scala.collection.immutable.Queue
object TravellingSalesmanBB {
  def inf = Double.PositiveInfinity
  val costMat = new Matrix[Double] (
    Vector(
      Vector[Double](inf,   7,   1,    8,   7),
      Vector[Double](  9, inf,   9,    2,   6),
      Vector[Double](  2,  12, inf,   11,  10),
      Vector[Double](  9,   9,   12, inf,   4),
      Vector[Double](  8,   1,   12,  10, inf)
    )
  )
  val xmat = new Matrix[Int] (
    Vector(
      Vector(0, 0, 1, 0, 0),
      Vector(0, 0, 0, 1, 0),
      Vector(1, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 1),
      Vector(0, 1, 0, 0, 0)
    )
  )

  val matrixTest = new Matrix[Double] (
    Vector(
      Vector[Double](0,   7,   1,    8,   7),
      Vector[Double](  9, 0,   9,    2,   6),
      Vector[Double](  2,  12, 0,   11,  10),
      Vector[Double](  9,   9,   12, 0,   4),
      Vector[Double](  8,   1,   12,  10, 0)
    )
  )

  def isFullCycle(destMat: Matrix[Int], cycle: List[List[(Int, Int)]]): Boolean =
    cycle.length == 1 && cycle(0).length == destMat.rowCount

  isFullCycle(xmat, List(List((0,3), (3,0)), List((1,4), (4,2), (2,1))))
}
