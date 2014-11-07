package transortation_problem

import common.Matrix
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import transportation_problem._

@RunWith(classOf[JUnitRunner])
class TransportSuite extends FunSuite{
  val -- = Double.NegativeInfinity

  test("nw corner test") {
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
    val expectedSolution = new solver.EquationSolution(
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

  test ("transport cycle builder") {
    val builder = new TransportCycleBuilder()
    val basis = Seq((0, 0), (0, 1), (1, 1), (2, 1), (2, 2), (2, 3))
    val route = builder.findCycle(basis, firstRow = 0, firstCol = 3)

    val expectedRoute = new Route(Seq(
      new CycleCell(0, 3, true, true, true),
      new CycleCell(0, 1, false, false),
      new CycleCell(2, 1, true, true),
      new CycleCell(2, 3, false, false)
    ))

    assert(route equals expectedRoute)
  }

  test("modify bsf") {
    val solver = new PMTransportationSolver(new NWCornerFSFinder())
    val d = IndexedSeq (
      ((0, 2), 1.0),
      ((0, 3), -4.0),
      ((1, 2), 0.0),
      ((1, 3), 0.0),
      ((1, 0), -2.0),
      ((2, 0), 3.0)
    )
    val bsf = new Matrix[Double](
      Vector(
        Vector( 5,  5, --,  --),
        Vector(--, 15, --,  --),
        Vector(--,  5, 10,  10)
      ))

    val expectedResult = new Matrix[Double](
      Vector(
        Vector( 5, --, --,   5),
        Vector(--, 15, --,  --),
        Vector(--, 10, 10,   5)
      ))

    val (result, cycle) = solver.modifyBsf(d, bsf)

    assert(result equals expectedResult)
  }

  test("solve") {
    val solver = new PMTransportationSolver(new NWCornerFSFinder())

    val tp = new TransportPack (
      costs = new Matrix[Double] (
        Vector(
          Vector(10, 7, 6, 8),
          Vector(5, 6, 5, 4),
          Vector(8, 7, 6, 7)
        )),
      prodCap = Vector[Double](31, 48, 38),
      consCap = Vector[Double](22, 34, 41, 20)
    )

    val expectedResult = new Matrix[Double](
      Vector(
        Vector(--, 31, --,  --),
        Vector(22,  3,  3,  20),
        Vector(--, --, 38,  --)
      ))

    val result = solver.solve(tp)

    assert(result equals expectedResult)
  }
}
