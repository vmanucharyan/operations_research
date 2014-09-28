package hungarian_method

import common.Matrix

import scala.collection.mutable

class Cost(val value: Double, val mark1: Boolean, var mark2: Boolean) {
  def this(value: Double) = this(value, false, false)
  override def toString = s"${value.toInt}${if (mark1) "*" else if (mark2) "'" else " "}"
}

object CostMatrix {
  def fromValues(values: Vector[Vector[Double]]) =
    new Matrix[Cost] (
      for (ri <- 0 until values.length) yield
        for (ci <- 0 until values(0).length) yield new Cost(values(ri)(ci))
    )

  def toString(mat: Matrix[Cost], markedRows: mutable.Set[Int], markedCols: mutable.Set[Int]) = {
    val sb = new mutable.StringBuilder()
    sb.append("\t")
    for (ci <- 0 until mat.colCount) {
      sb.append(if (markedCols.contains(ci)) "+\t\t" else "\t\t")
    }
    sb.append("\n")

    for (ri <- 0 until mat.rowCount) {
      sb.append(if (markedRows.contains(ri)) "+\t" else "\t")
      for (ci <- 0 until mat.colCount) {
        sb.append(s"${mat(ri, ci)}\t\t")
      }
      sb.append("\n")
    }

    sb.toString()
  }
}
