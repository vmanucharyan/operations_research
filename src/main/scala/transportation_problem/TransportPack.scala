package transportation_problem

import common.Matrix

class TransportPack(val costs: Matrix[Double],
                    val prodCap: Vector[Double],
                    val consCap: Vector[Double])
