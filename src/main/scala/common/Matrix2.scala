package common

class Matrix2[A](val rows: IndexedSeq[IndexedSeq[A]]) {
  val rowCount = rows.length
  val colCount = rows(0).length

  lazy val cols: IndexedSeq[IndexedSeq[A]] = {
    for (ri <- 0 until rows.length) yield
      for (ci <- 0 until rows(0).length) yield rows(ci)(ri)
  }

  def apply(ri: Int) = rows(ri)
  def apply(ri: Int, ci: Int) = rows(ri)(ci)

  def flatten = rows.flatten

  def mapRows[B](transform: IndexedSeq[A] => IndexedSeq[B]): Matrix2[B] =
    new Matrix2[B] (
      rows = for (row <- rows) yield transform(row)
    )

  def mapRowsIndexed[B](transform: (IndexedSeq[A], Int) => IndexedSeq[B]): Matrix2[B] =
    new Matrix2[B] (
      rows = for (ri <- 0 until rowCount) yield transform(rows(ri), ri)
    )

  def mapCols[B](transform: IndexedSeq[A] => IndexedSeq[B]): Matrix2[B] =
    Matrix2.fromCols[B] (
      cols = for (col <- cols) yield transform(col)
    )

  def mapColsIndexed[B](transform: (IndexedSeq[A], Int) => IndexedSeq[B]): Matrix2[B] =
    Matrix2.fromCols[B] (
      cols = for (ci <- 0 until colCount) yield transform(cols(ci), ci)
    )

  def map[B](transform: A => B): Matrix2[B] =
    new Matrix2[B] (
      rows =
        for (row <- rows) yield
          for (elem <- row) yield
            transform(elem)
    )

  def mapIndexed[B](transform: (A, Int, Int) => B): Matrix2[B] =
    new Matrix2[B] (
      for (ri <- 0 until rowCount) yield
        for (ci <- 0 until colCount) yield
          transform(rows(ri)(ci), ri, ci)
    )

  def max()(implicit ord: Ordering[A]): A = rows.flatten.max
  def min()(implicit ord: Ordering[A]): A = rows.flatten.min

  override def equals(that: Any) =
    that match {
      case thatMat: Matrix2[A] => this.flatten equals thatMat.flatten
      case _ => false
    }
}

object Matrix2 {
  def fromCols[A](cols: IndexedSeq[IndexedSeq[A]]): Matrix2[A] =
    new Matrix2 (
      for (ci <- 0 until cols.length) yield
        for (ri <- 0 until cols(0).length) yield cols(ri)(ci)
    )
}
