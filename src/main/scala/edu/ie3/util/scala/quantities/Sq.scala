/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.{Quantity, UnitOfMeasure}

object Sq {

  def create[A <: Quantity[A]](num: Number, unit: UnitOfMeasure[A]): A = {
    num match {
      case d: java.lang.Double =>
        unit.apply(d.doubleValue())
      case i: java.lang.Integer =>
        unit.apply(i.intValue())
    }
  }
}
