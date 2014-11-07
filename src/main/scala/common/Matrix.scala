package common

class Matrix[A](val rows: IndexedSeq[IndexedSeq[A]]) {
  def this (rows: Int, cols: Int, init: (Int, Int) => A) =
    this(
      for (ri <- 0 until rows) yield
        for (ci <- 0 until cols) yield
          init(ri, ci)
    )

  val rowCount = rows.length
  val colCount = rows(0).length

  lazy val cols: IndexedSeq[IndexedSeq[A]] = {
    for (ci <- 0 until colCount) yield
      for (ri <- 0 until rowCount) yield rows(ri)(ci)
  }

  def apply(ri: Int) = rows(ri)
  def apply(ri: Int, ci: Int) = rows(ri)(ci)

  def flatten = rows.flatten

  def setValue(value: A, row: Int, col: Int) =
    this.mapIndexed((v, r, c) => if (r == row && c == col) value else v)

  def setCol(col: IndexedSeq[A], colIndex: Int) =
    mapCol(colIndex, c => col)

  def setRow(row: IndexedSeq[A], rowIndex: Int) =
    mapRow(rowIndex, r => row)

  def mapRow(rowIndex: Int, transform: IndexedSeq[A] => IndexedSeq[A]): Matrix[A] =
    mapRowsIndexed((row, index) => if (index == rowIndex) transform(row) else row)

  def mapCol(colIndex: Int, transform: IndexedSeq[A] => IndexedSeq[A]): Matrix[A] =
    mapColsIndexed((col, index) => if (index == colIndex) transform(col) else col)

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

  def flatMap[B](transform: (A,Int,Int) => B, condition: (A, Int, Int) => Boolean = (_,_,_) => true): IndexedSeq[B] =
    for {
      ri <- 0 until rowCount
      ci <- 0 until colCount
      if condition(rows(ri)(ci), ri, ci)
    } yield transform(rows(ri)(ci), ri, ci)

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
      for (ri <- 0 until cols(0).length) yield
        for (ci <- 0 until cols.length) yield cols(ci)(ri)
    )
}
