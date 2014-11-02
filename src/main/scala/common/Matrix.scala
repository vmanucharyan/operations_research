package common

class Matrix[A](val rows: IndexedSeq[IndexedSeq[A]]) {
  val rowCount = rows.length
  val colCount = rows(0).length

  lazy val cols: IndexedSeq[IndexedSeq[A]] = {
    for (ri <- 0 until rows.length) yield
      for (ci <- 0 until rows(0).length) yield rows(ci)(ri)
  }

  def apply(ri: Int) = rows(ri)
  def apply(ri: Int, ci: Int) = rows(ri)(ci)

  def flatten = rows.flatten

  def mapRows[B](transform: IndexedSeq[A] => IndexedSeq[B]): Matrix[B] =
    new Matrix[B] (
      rows = for (row <- rows) yield transform(row)
    )

  def mapRowsIndexed[B](transform: (IndexedSeq[A], Int) => IndexedSeq[B]): Matrix[B] =
    new Matrix[B] (
      rows = for (ri <- 0 until rowCount) yield transform(rows(ri), ri)
    )

  def mapCols[B](transform: IndexedSeq[A] => IndexedSeq[B]): Matrix[B] =
    Matrix.fromCols[B] (
      cols = for (col <- cols) yield transform(col)
    )

  def mapColsIndexed[B](transform: (IndexedSeq[A], Int) => IndexedSeq[B]): Matrix[B] =
    Matrix.fromCols[B] (
      cols = for (ci <- 0 until colCount) yield transform(cols(ci), ci)
    )

  def map[B](transform: A => B): Matrix[B] =
    new Matrix[B] (
      rows =
        for (row <- rows) yield
          for (elem <- row) yield
            transform(elem)
    )

  def mapIndexed[B](transform: (A, Int, Int) => B): Matrix[B] =
    new Matrix[B] (
      for (ri <- 0 until rowCount) yield
        for (ci <- 0 until colCount) yield
          transform(rows(ri)(ci), ri, ci)
    )

  def max()(implicit ord: Ordering[A]): A = rows.flatten.max
  def min()(implicit ord: Ordering[A]): A = rows.flatten.min

  override def equals(that: Any) =
    that match {
      case thatMat: Matrix[A] => this.flatten equals thatMat.flatten
      case _ => false
    }
}

object Matrix {
  def fromCols[A](cols: IndexedSeq[IndexedSeq[A]]): Matrix[A] =
    new Matrix (
      for (ci <- 0 until cols.length) yield
        for (ri <- 0 until cols(0).length) yield cols(ri)(ci)
    )
}
