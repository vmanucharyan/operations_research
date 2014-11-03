package transportation_problem

import common.IndexedSeqExt._
import common.Matrix

class NWCornerFSFinder extends FeasibleSolutionFinder {
  override def find(problem: TransportPack): Matrix[Double] = {
    def nwCorner(mat: Matrix[Double]): (Int, Int) = {
      def iter(row: Int, col: Int): (Int, Int) =
        if (row >= mat.rowCount) (-1, -1)
        else if (mat(row, col) == 0) (row, col)
        else iter(
          if (col == mat.colCount - 1) row + 1 else row,
          if (col == mat.colCount - 1) 0 else col + 1
        )

      iter(0, 0)
    }

    def iter(p: TransportPack): TransportPack = {
      val (nwRow, nwCol) = nwCorner(p.costs)

      if (nwRow == -1) p
      else {
        val min = math.min(p.consCap(nwCol), p.prodCap(nwRow))
        val s = p.prodCap.updated(nwRow, p.prodCap(nwRow) - min)
        val d = p.consCap.updated(nwCol, p.consCap(nwCol) - min)

        val costs =
          if (s(nwRow) == 0)
            p.costs.mapRow(
              nwRow,
              (row) => row.mapIndexed((v, i) =>
                if (i == nwCol) min
                else if (v == 0) nan
                else v
              )
            )
          else
            p.costs.mapCol(
              nwCol,
              (col) => col.mapIndexed((v, i) =>
                if (i == nwRow) min
                else if (v == 0) nan
                else v
              )
            )

        iter(new TransportPack(costs, s, d))
      }
    }
    val res = iter(
      new TransportPack(
        costs = new Matrix(problem.costs.rowCount, problem.costs.colCount, (_,_) => 0.0),
        consCap = problem.consCap,
        prodCap = problem.prodCap
      )
    )

    res.costs
  }
}
