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
      def updateMatrix(nwRow: Int, nwCol: Int, s: IndexedSeq[Double], d: IndexedSeq[Double]) = {
        val min = math.min(p.consCap(nwCol), p.prodCap(nwRow))
        val rowsUpdated =
          if (s(nwRow) == 0)
            p.costs.mapRow(
              nwRow,
              (row) => row.mapIndexed((v, i) =>
                if (i == nwCol) min
                else if (v == 0) --
                else v
              ))
            else p.costs

        if (d(nwCol) == 0)
          rowsUpdated.mapCol(
            nwCol,
            (col) => col.mapIndexed((v, i) =>
              if (i == nwRow) min
              else if (v == 0) --
              else v
            ))
        else
          rowsUpdated
      }

      val (nwRow, nwCol) = nwCorner(p.costs)

      if (nwRow == -1) p
      else {
        val min = math.min(p.consCap(nwCol), p.prodCap(nwRow))
        val s = p.prodCap.updated(nwRow, p.prodCap(nwRow) - min)
        val d = p.consCap.updated(nwCol, p.consCap(nwCol) - min)
        val costs = updateMatrix(nwRow, nwCol, s, d)

        iter(new TransportPack(costs, s, d))
      }
    }

    def checkBsf(matrix: Matrix[Double]): Matrix[Double] = {
      val m = matrix.colCount
      val n = matrix.rowCount
      val x = matrix.flatten.count(p => !isNonExistent(p))

      if ((m + n - 1) == x) matrix
      else {
        val (v, vr, vc) = matrix.flatMap((v, row, col) => (v, row, col), (v, row, col) => isNonExistent(v)).head
        checkBsf(matrix.setValue(0.0, vr, vc))
      }
    }

    val res = iter(
      new TransportPack(
        costs = new Matrix(problem.costs.rowCount, problem.costs.colCount, (_,_) => 0.0),
        consCap = problem.consCap,
        prodCap = problem.prodCap
      )
    )

    checkBsf(res.costs)
  }
}
