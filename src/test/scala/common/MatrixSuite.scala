package common

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixSuite extends FunSuite {
  test("Matrix from rows equals to same matrix created from cols") {
    val matRows = new Matrix2[Int](
      Vector(
        Vector(1, 2, 3, 4),
        Vector(5, 6, 7, 8),
        Vector(9, 8, 7, 6),
        Vector(5, 4, 3, 2)
      )
    )

    val matCols = Matrix2.fromCols[Int](
      Vector(
        Vector(1, 5, 9, 5),
        Vector(2, 6, 8, 4),
        Vector(3, 7, 7, 3),
        Vector(4, 8, 6, 2)
      )
    )

    assert(matRows == matCols)
  }
}
