package transortation_problem

import common.Matrix
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import transportation_problem.{PMTransportationSolver, NWCornerFSFinder, TransportPack}

@RunWith(classOf[JUnitRunner])
class TransportSuite extends FunSuite{
  val -- = Double.NegativeInfinity

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
        Vector( 5,  5, --,  --),
        Vector(--, 15, --,  --),
        Vector(--,  5, 10,  10)
      ))

    val result = nwCorner.find(tp)
    assert(expectedResult equals result)
  }

  test("equation solver") {
    val solver = new PMTransportationSolver(new NWCornerFSFinder())
    val expectedSolution = new solver.EqSolution(
      u = Map(1 -> 0.0, 2 -> 3.0, 3 -> -1.0),
      v = Map(1 -> 3.0, 2 -> 3.0, 3 -> 4.0, 4 -> 5.0)
    )

    val system = IndexedSeq(
      new solver.Equation(1, 1, 3),
      new solver.Equation(1, 2, 3),
      new solver.Equation(2, 2, 6),
      new solver.Equation(3, 2, 2),
      new solver.Equation(3, 3, 3),
      new solver.Equation(3, 4, 4)
    )

    val solution = solver.solveEqSystem(system)

    assert(solution.v equals expectedSolution.v)
    assert(solution.u equals expectedSolution.u)
  }

  test("improve solution") {
    val solver = new PMTransportationSolver(new NWCornerFSFinder())
    solver.solve(tp)

    assert(false)
  }
}
