package hungarian_method

class Matrix[T](val rows: Vector[Vector[T]]) {
  val colCount = rows(0).length
  val rowCount = rows.length

  def this(rows: IndexedSeq[IndexedSeq[T]]) =
    this((for (row <- rows) yield row.toVector).toVector)

  def this(rowCount: Int, colCount: Int, initializer: (Int, Int) => T) =
    this(
      for (ri <- 0 until rowCount) yield
        for (ci <- 0 until colCount) yield initializer(ri, ci)
    )

  def apply(row: Int, col: Int) = rows(row)(col)
  def apply(row: Int) = rows(row)

  def flattenRowBased() =
    (for (ri <- 0 until rowCount; ci <- 0 until colCount) yield rows(ri)(ci)).toVector

  def map[B](func: (T, Int, Int) => B) =
    new Matrix[B](
      for (ri <- 0 until rowCount) yield
        for (ci <- 0 until colCount) yield
          func(rows(ri)(ci), ri, ci)
    )

  def zip[B](other: Matrix[B]) =
    new Matrix[(T, B)] (
      for (ri <- 0 until rowCount) yield rows(ri).zip(other.rows(ri))
    )

  def zipWithIndexes() =
    new Matrix[(T, Int, Int)](
      for (ri <- 0 until rowCount) yield
        for (ci <- 0 until colCount) yield (rows(ri)(ci), ri, ci)
    )

  def row(ri: Int): Vector[T] =
    (for (ci <- 0 until colCount) yield rows(ri)(ci)).toVector

  def col(ci: Int): Vector[T] =
    (for (ri <- 0 until rowCount) yield rows(ri)(ci)).toVector

  def rowFiltered(ri: Int, func: (T) => Boolean) =
    (for (ci <- 0 until colCount if func(rows(ri)(ci))) yield (rows(ri)(ci), ci)).toVector

  def colFiltered(ci: Int, func: (T) => Boolean) =
    (for (ri <- 0 until rowCount if func(rows(ri)(ci))) yield (rows(ri)(ci), ri)).toVector

  def setRow(ri: Int, newRow: Vector[T]) =
    mapRows((row, index) => if (index == ri) newRow else row)

  def setCol(ci: Int, newCol: Vector[T]) =
    mapCols((col, index) => if (index == ci) newCol else col)

  def mapRows(func: (Vector[T], Int) => Vector[T]) =
    new Matrix[T] (
      for (ri <- 0 until rowCount) yield
        func(row(ri), ri)
    )

  def mapCols(func: (Vector[T], Int) => Vector[T]) =
    Matrix.fromCols[T] (
      for (ci <- 0 until colCount) yield
        func(col(ci), ci)
    )

  def find(predicate: (T, Int, Int) => Boolean) =
    for (ri <- 0 until rowCount; ci <- 0 until colCount if predicate(rows(ri)(ci), ri, ci))
      yield (rows(ri)(ci), ri , ci)

  def count(predicate: (T, Int, Int) => Boolean) =
    (for (ri <- 0 until rowCount; ci <- 0 until colCount) yield
      if (predicate(rows(ri)(ci), ri, ci)) 1 else 0).sum

  def min[B](e: (T) => B, condition: (T, Int, Int) => Boolean = (_,_,_) => true)(implicit m: Ordering[B]) = {
    val filtered =
      for (ri <- 0 until rowCount; ci <- 0 until colCount if condition(rows(ri)(ci), ri, ci)) yield
        (e(rows(ri)(ci)), ri, ci)

    filtered.min
  }

  def max[B](e: (T) => B, condition: (T, Int, Int) => Boolean = (_,_,_) => true)(implicit m: Ordering[B]) = {
    val filtered =
      for (ri <- 0 until rowCount; ci <- 0 until colCount if condition(rows(ri)(ci), ri, ci)) yield
        (e(rows(ri)(ci)), ri, ci)

    filtered.max
  }

  override def equals(other: Any) : Boolean = {
    def elementsAreEqual(mat: Matrix[T]) =
      mat.flattenRowBased() equals mat.flattenRowBased()

    if (!other.isInstanceOf[hungarian_method.Matrix[T]])
      false
    else
      elementsAreEqual(other.asInstanceOf[Matrix[T]])
  }

  override def toString = {
    val sb = new StringBuilder()
    for (ri <- 0 until rowCount) {
      for (ci <- 0 until colCount) {
        sb.append(s"${rows(ri)(ci)}\t\t")
      }
      sb.append("\n")
    }

    sb.toString()
  }
}

object Matrix {
  def fromCols[T](cols: IndexedSeq[IndexedSeq[T]]) =
    new Matrix[T] (
      for (ri <- 0 until cols(0).length) yield
        for (ci <- 0 until cols.length) yield cols(ci)(ri)
    )
}
