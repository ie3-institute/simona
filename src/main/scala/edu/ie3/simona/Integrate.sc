import squants.energy.Kilowatts
import squants.time.{Hours, TimeDerivative, TimeIntegral}
import squants.Quantity

//def integrate[Q <: Quantity[Q] with TimeDerivative[QI], QI <: Quantity[QI] with TimeIntegral[Q]](
//    value: Q,
//): Unit = {
//  value * Hours(1)
//}
//val power = Kilowatts(1)
//integrate(
//  power,
//)


import squants.energy.{Kilowatts, Energy}
import squants.time.{Hours, TimeDerivative, TimeIntegral}
import squants.Quantity

def integrate[Q <: Quantity[Q] with TimeDerivative[QI], QI <: Quantity[QI] with TimeIntegral[Q]](
    value: Q,
)(implicit ev: QI =:= Energy): QI = { // Assuming the integral of Power over time gives Energy
  value * Hours(1) // This should now return a QI, which we're asserting is a type of Energy
}

val power = Kilowatts(1)
val energy: Energy = integrate(power) // Here, we're explicitly stating that we expect an Energy back