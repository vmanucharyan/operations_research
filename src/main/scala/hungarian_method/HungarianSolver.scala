package hungarian_method

import common.MatrixOld

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, MutableList}

class HungarianSolver ( matrix: Vector[Vector[Double]],
                        private val _traceCallback: (MatrixOld[Cost], String, mutable.Set[Int], mutable.Set[Int]) => Unit) {

  def this (matrix: MatrixOld[Double]) = this(matrix.rows, (_,_,_,_) => {})

  private val _inputMatrix = CostMatrix.fromValues(matrix)
  private val _n = _inputMatrix.rowCount

  def solve(maximize: Boolean) = {
    def maximizationCostMatrix(mat: MatrixOld[Cost]) = {
      val (max, _, _) = mat.zipWithIndexes().flat().maxBy{ case (c: Cost, ri: Int, ci: Int) => c.value }
      mat.map((c, _, _) => { new Cost(-c.value + max.value, false, false) })
    }

    def equivalentCostMatrix(mat: MatrixOld[Cost]) = {
      mat.mapCols (
        (col, index) => {
          val l = col.minBy((cost: Cost) => cost.value)
          col.map((cost) => new Cost(cost.value - l.value, cost.mark1, cost.mark2))
        }
      ).mapRows(
          (row, index) => {
            val l = row.minBy((cost: Cost) => cost.value)
            row.map((cost: Cost) => new Cost(cost.value - l.value, cost.mark1, cost.mark2))
          }
        )
    }

    val input = if (maximize) maximizationCostMatrix(_inputMatrix) else _inputMatrix
    val eq = equivalentCostMatrix(input)
    var cs = findSIZ(eq)

    var k = cs.count((cost, _, _) => cost.mark1)

    var numIter = 1
    while (k < _n) {
      cs = performIteration(cs)
      k += 1
      numIter += 1
    }

    val xopt = optimalMatrix(cs)

    (xopt, optimalValue(_inputMatrix, xopt))
  }

  def findSIZ(mat: MatrixOld[Cost]) = {
    val marked = mutable.MutableList[(Int, Int)]()
    mat.mapCols (
      (col, colIndex) => col.zipWithIndex.map {
        case (cost, rowIndex) =>
          val mark1 =
            (cost.value == 0) &&
              (marked.count {case (ri, ci) => (rowIndex == ri) || (colIndex == ci)} == 0)

          if (mark1)
            marked += ((rowIndex, colIndex))

          new Cost(cost.value, mark1, cost.mark2)
      }
    )
  }

  def optimalMatrix(mat: MatrixOld[Cost]) =
    mat.map((cost, rowIndex, colIndex) => if (cost.mark1) 1 else 0)

  def optimalValue(c: MatrixOld[Cost], x: MatrixOld[Int]) = {
    c.map((c, ri, ci) => if (!c.value.isInfinity) c.value * x(ri, ci) else 0).
      flat().
      sum
  }

  def performIteration(mat: MatrixOld[Cost]) = {
    def findColsWithZeroStar(mat: MatrixOld[Cost]) = {
      val res = mutable.Set.empty[Int]
      for (ci <- 0 until mat.colCount)
        if (mat.col(ci).count((c) => c.mark1) > 0)
          res += ci
      res
    }

    var res = mat

    val markedCols = findColsWithZeroStar(mat)
    val markedRows = mutable.Set.empty[Int]

    var zeroStarInRowFound = false
    do {
      val (m, zeroRow, zeroCol) = findUnmarkedZero(res, markedRows, markedCols)

      res = m.map (
        (cost, r, c) =>
          if (r == zeroRow && c == zeroCol) new Cost(cost.value, false, true)
          else cost
      )

      val zsInRow = res.rowFiltered(zeroRow, (c) => c.mark1)
      if (zsInRow.length > 0) {
        val (_,zeroStarCol) = zsInRow(0)

        markedRows += zeroRow
        markedCols -= zeroStarCol

        zeroStarInRowFound = true
      }
      else {
        val lseq = buildLSequence(res, zeroRow, zeroCol)

        res = res.map(
          (c, ri, ci) => {
            if (lseq.contains((ri, ci)))
              if (c.mark1) new Cost(0, false, false)
              else new Cost(0, true, false)
            else
            if (c.mark2) new Cost(0, false, false)
            else c
          }
        )

        zeroStarInRowFound = false
      }
    }
    while (zeroStarInRowFound)

    res
  }

  def findUnmarkedZero(mat: MatrixOld[Cost], markedRows: mutable.Set[Int], markedCols: mutable.Set[Int]) = {
    def makeZero(mat: MatrixOld[Cost], markedRows: mutable.Set[Int], markedCols: mutable.Set[Int]) = {
      val (h, ri, ci) = mat.min(
        (c) =>
          c.value, (c, ri, ci) => c.value > 0 &&
          !markedRows.contains(ri) &&
          !markedCols.contains(ci) &&
          !c.value.isInfinity
      )

      val transformed =
        mat.mapRows (
          (row, rowIndex) =>
            if (!markedRows.contains(rowIndex)) row.map((c) => new Cost(c.value - h, c.mark1, c.mark2))
            else row
        ).mapCols (
          (col, colIndex) =>
            if (markedCols.contains(colIndex)) col.map((c) => new Cost(c.value + h, c.mark1, c.mark2))
            else col
        )

      (transformed, ri, ci)
    }

    val zeros = mat.find(
      (c, ri, ci) =>  c.value == 0 && !c.mark1 && !c.mark2 &&
        !markedRows.contains(ri) && !markedCols.contains(ci)
    )

    if (zeros.length == 0) {
      val (trMat, zeroRI, zeroCi) = makeZero(mat, markedRows, markedCols)
      (trMat, zeroRI, zeroCi)
    }
    else {
      val (_, rowIndex, colIndex) = zeros(0)
      (mat, rowIndex, colIndex)
    }
  }

  def buildLSequence(mat: MatrixOld[Cost], zeroRow: Int, zeroCol: Int) = {
    def loop(mat: MatrixOld[Cost], currRow: Int, currCol: Int, sequence: List[(Int, Int)]): List[(Int, Int)] =
      if (mat(currRow, currCol).mark2) {
        val newElemRow = mat.col(currCol).indexWhere((c) => c.mark1)
        if (newElemRow != -1)
          loop(mat, newElemRow, currCol, sequence :+ (newElemRow, currCol))
        else
          sequence
      }
      else {
        val newElemCol = mat.row(currRow).indexWhere((c) => c.mark2)
        if (newElemCol != -1)
          loop(mat, currRow, newElemCol, sequence :+ (currRow, newElemCol))
        else
          sequence
      }

    loop(mat, zeroRow, zeroCol, List((zeroRow, zeroCol)))
  }
}

