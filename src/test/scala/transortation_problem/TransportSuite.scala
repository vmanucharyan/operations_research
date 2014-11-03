package transortation_problem

import common.Matrix
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import transportation_problem.{NWCornerFSFinder, TransportPack}

@RunWith(classOf[JUnitRunner])
class TransportSuite extends FunSuite{
  val nan = Double.NegativeInfinity

  val nwCorner = new NWCornerFSFinder()
  val tp = new TransportPack (
    costs = new Matrix[Double] (
      Vector(
        Vector(3, 3, 5, 1),
        Vector(4, 6, 7, 8),
        Vector(5, 2, 3, 4)
      )),
    prodCap = Vector[Double](10, 15, 25),
    consCap = Vector[Double](5, 25, 10, 10)
  )

  test("nw corner test") {
    val expectedResult = new Matrix[Double](
      Vector(
        Vector(  5,   5, nan, nan),
        Vector(nan,  15, nan, nan),
        Vector(nan,   5,  10,  10)
      ))

    val result = nwCorner.find(tp)
    assert(expectedResult equals result)
  }
}
