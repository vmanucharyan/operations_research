import hungarian_method.HungarianSolver
object HungarianMethod {
  val matrix : Vector[Vector[Double]] =
    Vector(
      Vector(9, 9, 9, 9, 9),
      Vector(9, 8, 1, 8, 8),
      Vector(9, 5, 7, 4, 9),
      Vector(9, 6, 8, 9, 3),
      Vector(9, 3, 9, 4, 3)
    )

  val matrix7 : Vector[Vector[Double]] =
    Vector(
      Vector(11, 4, 11,  6, 11),
      Vector(7,  5,  6,  7, 12),
      Vector(9,  7,  8, 10, 10),
      Vector(9, 11,  6, 10,  9),
      Vector(7, 10,  4,  8,  8)
    )

  val example : Vector[Vector[Double]] =
    Vector(
      Vector( 5, 6, 7, 1),
      Vector(10, 4, 6, 7),
      Vector( 8, 5, 3, 5),
      Vector(12, 5, 9, 8)
    )

  val example2 : Vector[Vector[Double]] =
    Vector(
      Vector(1, 0, 1, 4),
      Vector(0, 2, 1, 1),
      Vector(2, 2, 5, 0),
      Vector(3, 1, 0, 5)
    )
  val solver =
    new HungarianSolver (
      matrix,
      (mat, msg, mr, mc) => {}
    )
  val (res, opt) = solver.solve(maximize = true)










































}
