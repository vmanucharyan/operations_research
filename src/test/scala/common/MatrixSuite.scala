package common

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixSuite extends FunSuite {
  val testMatrix = new Matrix[Int](
    Vector(
      Vector(1, 2, 3),
      Vector(5, 6, 7),
      Vector(9, 8, 7),
      Vector(5, 4, 3)
    )
  )

  test("Matrix from cols") {
    val matCols = Matrix.fromCols[Int](
      Vector(
        Vector(1, 5, 9, 5),
        Vector(2, 6, 8, 4),
        Vector(3, 7, 7, 3)
      )
    )

    assert(testMatrix == matCols)
  }

  test("Matrix cols") {
    val matCols =
      Vector(
        Vector(1, 5, 9, 5),
        Vector(2, 6, 8, 4),
        Vector(3, 7, 7, 3)
      )

    assert(testMatrix.cols equals matCols)
  }
}
