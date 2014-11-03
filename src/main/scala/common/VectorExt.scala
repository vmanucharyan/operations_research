package common

object IndexedSeqExt {
  implicit class IndexedSeqImpl[A](vec: IndexedSeq[A]) {
    def mapIndexed[B](transform: (A, Int) => B): IndexedSeq[B] =
      for (i <- 0 until vec.length) yield transform(vec(i), i)
  }
}
