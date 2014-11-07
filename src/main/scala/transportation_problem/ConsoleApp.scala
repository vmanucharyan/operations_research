package transportation_problem

import common.Matrix

object ConsoleApp {
  def main(args: Array[String]): Unit = {
    task1()
    task2()
  }

  def task1(): Unit = {
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

    System.out.println("----------------------------------------------------------")
    System.out.println("Результат:")
    printMatrix(result)
  }

  def task2(): Unit = {
    val callback =
      (cycle: Route, matrix: Matrix[Double], iter: Int) =>
        printState(cycle, matrix, iter)

    val solver = new PMTransportationSolver(new NWCornerFSFinder(), callback)
    val tp2 = new TransportPack(
      costs = new Matrix[Double](
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

    val result2 = solver.solve(tp2)
    System.out.println("----------------------------------------------------------")
    System.out.println("Результат:")
    printMatrix(result2)
  }

  def printState(cycle: Route, matrix: Matrix[Double], iter: Int): Unit = {
    System.out.println("----------------------------------------------------------")
    System.out.println(f"Итерация #$iter%d")
    System.out.println()
    printMatrix(matrix)
    System.out.println()
    printCycle(cycle)
  }
}
