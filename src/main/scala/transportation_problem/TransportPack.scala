package transportation_problem

import common.Matrix

case class TransportPack(costs: Matrix[Double],
                         prodCap: Vector[Double],
                         consCap: Vector[Double])
