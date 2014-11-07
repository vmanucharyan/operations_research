package transportation_problem

import common.Matrix

case class CycleCell(row: Int,
                     col: Int,
                     markPlus: Boolean,
                     reachedByCol: Boolean,
                     isFirst: Boolean = false)
{
  def canReach(that: CycleCell) =
    if (reachedByCol) this.row == that.row
    else this.col == that.col
}

case class Route(seq: Seq[CycleCell]) {
  def containsCell(row: Int, col: Int) =
    seq.exists(cell => cell.row == row && cell.col == col)

  def union(that: Route): Route =
    new Route(this.seq ++ that.seq)

  def append(cell: CycleCell): Route =
    new Route(this.seq :+ cell)
}

case class RouteTree(route: Route, children: Iterable[RouteTree])

class TransportCycleBuilder {
  def findCycle(basisCells: Iterable[(Int, Int)], firstRow: Int, firstCol: Int): Route = {
    def waysToGo(route: Route, cell: CycleCell): Iterable[CycleCell] = {
      val withoutSelf = basisCells filter { case (row, col) => !(row == cell.row && col == cell.col) }
      val filteredCells = withoutSelf filter { case (row, col) => !route.containsCell(row, col) }

      val cellsToGo =
        if (cell.isFirst) basisCells filter { case (row, col) => row == cell.row || col == cell.col }
        else if (cell.reachedByCol) filteredCells filter { case (row, col) => row == cell.row }
        else filteredCells filter { case (row, col) => col == cell.col }

      cellsToGo map {
        case (row, col) =>
          new CycleCell(row, col, !cell.markPlus, cell.col == col )
      }
    }

    def canReachFirst(route: Route, cell: CycleCell) = route.seq.nonEmpty && cell.canReach(route.seq(0))

    def findCycle(route: Route, cell: CycleCell): Option[Route] = {
      val cellsToGo = waysToGo(route, cell)

//      System.out.println("======================")
//      System.out.println(cellsToGo)
//      System.out.println(route)

      if (canReachFirst(route, cell))
        Some(route append cell)
      else if (cellsToGo.isEmpty)
        None
      else {
        val possibleRoutes = cellsToGo map (c => findCycle(route append cell, c))
        possibleRoutes find (route => route.isDefined) getOrElse None
      }
    }

    val firstCycle = new CycleCell(
      firstRow,
      firstCol,
      markPlus = true,
      reachedByCol = true,
      isFirst = true
    )

    findCycle(new Route(Seq()), firstCycle).get
  }
}


