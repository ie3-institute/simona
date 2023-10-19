import edu.ie3.simona.exceptions.QuantityException
import squants.{Energy, Power}
import squants.energy.Kilowatts
import squants.time.{Hours, TimeDerivative, TimeIntegral}

import javax.measure.Quantity
import scala.collection.mutable

def integrate[Q <: Power with TimeDerivative[Energy], QI <: Quantity[QI] with TimeIntegral[Q]](
    values: Map[Long, Q],
    windowStart: Long,
    windowEnd: Long,
): Unit = {
  print("Integrating")
}

val values = Map(
  1L -> Kilowatts(1),
  2L -> Kilowatts(1),
  3L -> Kilowatts(1),
)

integrate(
  values,
  1L,
  3L,
)
