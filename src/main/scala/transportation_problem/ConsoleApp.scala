package transportation_problem

import common.Matrix

object ConsoleApp {
  def printMatrix(matrix: Matrix[Double]) = {
    for (row <- matrix.rows) {
      for (e <- row) {
        if (!isNonExistent(e)) System.out.print(f"$e%1.0f\t")
        else System.out.print(f"--\t")
      }
      System.out.println()
    }
  }

  def printCycle(cycle: Route): Unit = {
    for (i <- 0 until cycle.seq.size - 1)
      System.out.print(f"(${cycle.seq(i).row}, ${cycle.seq(i).col}) ->")
    System.out.println(f"(${cycle.seq.last.row}, ${cycle.seq.last.col})")
  }

  def printState(cycle: Route, matrix: Matrix[Double], iter: Int): Unit = {
    System.out.println(f"Итерация #$iter%d")
    System.out.println()
    printMatrix(matrix)
    System.out.println()
    printCycle(cycle)
  }

  def main(args: Array[String]): Unit = {
    val callback =
      (cycle: Route, matrix: Matrix[Double], iter: Int) =>
        printState(cycle, matrix, iter)

    val solver = new PMTransportationSolver(new NWCornerFSFinder(), callback)
    val tp = new TransportPack (
      costs = new Matrix[Double] (
        Vector(
          Vector(7, 8, 4, 3),
          Vector(6, 2, 5, 4),
          Vector(4, 3, 7, 3)
        )),

      prodCap = Vector[Double](270, 90, 340),
      consCap = Vector[Double](90, 90, 200, 320)
    )

    val result = solver.solve(tp)
    System.out.println("Результат:")
    printMatrix(result)

    val tp2 = new TransportPack (
      costs = new Matrix[Double] (
        Vector(
          Vector(7, 7, 4, 6, 5),
          Vector(3, 8, 1, 8, 8),
          Vector(5, 5, 7, 4, 1),
          Vector(7, 6, 8, 6, 3),
          Vector(4, 9, 2, 4, 3)
        )),

      prodCap = Vector[Double](1, 1, 1, 1, 1),
      consCap = Vector[Double](1, 1, 1, 1, 1)
    )

    solver.solve(tp2)

    val result2 = solver.solve(tp2)
    System.out.println("Результат:")
    printMatrix(result2)
  }
}
