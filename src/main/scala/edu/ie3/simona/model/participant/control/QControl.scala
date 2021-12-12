/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.control

import edu.ie3.datamodel.models.input.system.characteristic
import edu.ie3.datamodel.models.input.system.characteristic.ReactivePowerCharacteristic
import edu.ie3.simona.exceptions.QControlException
import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.util.quantities.PowerSystemUnits.{MEGAVAR, MEGAWATT, PU}

import javax.measure.quantity.{Dimensionless, Power}
import tech.units.indriya.{AbstractUnit, ComparableQuantity}
import tech.units.indriya.quantity.Quantities

import scala.collection.SortedSet
import scala.collection.immutable.TreeSet
import scala.jdk.CollectionConverters._
import scala.math._

sealed trait QControl {
  protected val _cosPhiMultiplication
      : (Double, ComparableQuantity[Power]) => ComparableQuantity[Power] =
    (cosPhi: Double, p: ComparableQuantity[Power]) =>
      if ((cosPhi - 1).abs < 0.0000001) {
        p.multiply(0d).to(MEGAVAR)
      } else {
        /* q = p * tan( phi ) = p * tan( acos( cosphi )) */
        p.multiply(tan(acos(cosPhi))).to(MEGAVAR)
      }

  /** Obtain the function, that transfers active into reactive power
    *
    * @param sRated
    *   Rated apparent power
    * @param cosPhiRated
    *   Rated power factor
    * @param nodalVoltage
    *   Nodal voltage for parameterization of the function
    * @return
    *   The function
    */
  def activeToReactivePowerFunc(
      sRated: ComparableQuantity[Power],
      cosPhiRated: Double,
      nodalVoltage: ComparableQuantity[Dimensionless]
  ): ComparableQuantity[Power] => ComparableQuantity[Power]
}

/** Object to create a [[QControl]]. Currently the following QControls
  * characteristics are supported.
  *   - cosine-Phi-Fixed
  *   - cosine-Phi(P)
  *   - Q(v)
  */
object QControl {

  def apply(varCharacteristic: ReactivePowerCharacteristic): QControl =
    varCharacteristic match {
      case cosPhiFixed: characteristic.CosPhiFixed =>
        if (cosPhiFixed.getPoints.size() > 1)
          throw new QControlException(
            s"Got an invalid definition of fixed power factor: $cosPhiFixed. It may only contain one coordinate"
          )
        CosPhiFixed(
          cosPhiFixed.getPoints.first().getY.to(PU).getValue.doubleValue()
        )
      case cosPhiP: characteristic.CosPhiP =>
        CosPhiP(
          TreeSet.from(
            cosPhiP.getPoints.asScala.map(point =>
              XYPair[Dimensionless, Dimensionless](point.getX, point.getY)
            )
          )
        )
      case qv: characteristic.QV =>
        QV(
          TreeSet.from(
            qv.getPoints.asScala
              .map(point =>
                XYPair[Dimensionless, Dimensionless](point.getX, point.getY)
              )
              .toSeq
          )
        )
      case unknownType =>
        throw new QControlException(
          s"Cannot parse unknown characteristic type $unknownType. Please provide a valid ReactivePowerCharacteristic"
        )
    }

  /** CosPhiFixed var characteristic
    *
    * @param cosPhi
    *   the fixed cosPhi
    */
  final case class CosPhiFixed private (cosPhi: Double) extends QControl {

    /** Obtain the function, that transfers active into reactive power
      *
      * @param sRated
      *   Rated apparent power
      * @param cosPhiRated
      *   Rated power factor
      * @param nodalVoltage
      *   Nodal voltage for parameterization of the function
      * @return
      *   The function
      */
    override def activeToReactivePowerFunc(
        sRated: ComparableQuantity[Power],
        cosPhiRated: Double,
        nodalVoltage: ComparableQuantity[Dimensionless]
    ): ComparableQuantity[Power] => ComparableQuantity[Power] = {
      activePower: ComparableQuantity[Power] =>
        _cosPhiMultiplication(cosPhi, activePower)
    }
  }

  /** Voltage dependant var characteristic
    *
    * @param xyCoordinates
    *   the characteristic as sequence of (x,y)
    */
  final case class QV private (
      xyCoordinates: SortedSet[XYPair[Dimensionless, Dimensionless]]
  ) extends QControl
      with Characteristic[Dimensionless, Dimensionless] {

    /** Returns the resulting reactive power for the requested voltage level
      * value. The conversion to abstract unit [[AbstractUnit.ONE]] is necessary
      * because the interpolation always returns a relative factor of the rated
      * reactive power.
      *
      * @param vInPu
      *   the voltage in p.u.
      * @param qMax
      *   the maximum reactive power the load or in-feed is able to provide @
      *   the current operation point
      * @return
      *   the resulting reactive power q
      */
    def q(
        vInPu: ComparableQuantity[Dimensionless],
        qMax: ComparableQuantity[Power]
    ): ComparableQuantity[Power] =
      qMax
        .multiply(interpolateXy(vInPu)._2.to(AbstractUnit.ONE))
        .asType(classOf[Power])

    /** Obtain the function, that transfers active into reactive power
      *
      * @param sRated
      *   Rated apparent power
      * @param cosPhiRated
      *   Rated power factor
      * @param nodalVoltage
      *   Nodal voltage for parameterization of the function
      * @return
      *   The function
      */
    override def activeToReactivePowerFunc(
        sRated: ComparableQuantity[Power],
        cosPhiRated: Double,
        nodalVoltage: ComparableQuantity[Dimensionless]
    ): ComparableQuantity[Power] => ComparableQuantity[Power] = {
      activePower: ComparableQuantity[Power] =>
        val qMaxFromP = Quantities.getQuantity(
          sqrt(
            pow(sRated.to(MEGAWATT).getValue.doubleValue, 2) - pow(
              activePower.to(MEGAWATT).getValue.doubleValue,
              2
            )
          ),
          MEGAVAR
        )

        val qFromCharacteristic =
          q(nodalVoltage, sRated.multiply(sin(acos(cosPhiRated))).to(MEGAVAR))
        qMaxPossible(qMaxFromP, qFromCharacteristic)
    }

    /** Limit the reactive power proposed by the characteristic to not violate
      * apparent power constraints
      *
      * @param qMaxFromP
      *   Maximum permissible reactive power to not violate apparent power
      *   constraint
      * @param qFromCharacteristic
      *   Reactive power proposed by the characteristic
      * @return
      *   Properly limited reactive power
      */
    private def qMaxPossible(
        qMaxFromP: ComparableQuantity[Power],
        qFromCharacteristic: ComparableQuantity[Power]
    ): ComparableQuantity[Power] =
      if (
        abs(qFromCharacteristic.to(MEGAVAR).getValue.doubleValue())
          >= abs(qMaxFromP.to(MEGAVAR).getValue.doubleValue())
      )
        qMaxFromP.multiply(
          copySign(1, qFromCharacteristic.getValue.doubleValue())
        )
      else
        qFromCharacteristic
  }

  /** Power dependant var characteristic
    *
    * @param xyCoordinates
    *   the characteristic as sequence of (x,y)
    */
  final case class CosPhiP private (
      xyCoordinates: SortedSet[XYPair[Dimensionless, Dimensionless]]
  ) extends QControl
      with Characteristic[Dimensionless, Dimensionless] {

    /** Returns the requested cosine phi value for a provided power value
      * (p/sRated) in p.u. If the cosine phi cannot be found for the requested
      * value, it is interpolated.
      *
      * @param pInPu
      *   the ratio between the current power in-feed/load and the rated power
      * @return
      *   the cosine phi for the requested p.u. value
      */
    def cosPhi(
        pInPu: ComparableQuantity[Dimensionless]
    ): ComparableQuantity[Dimensionless] =
      interpolateXy(pInPu)._2

    /** Obtain the function, that transfers active into reactive power
      *
      * @param sRated
      *   Rated apparent power
      * @param cosPhiRated
      *   Rated power factor
      * @param nodalVoltage
      *   Nodal voltage for parameterization of the function
      * @return
      *   The function
      */
    override def activeToReactivePowerFunc(
        sRated: ComparableQuantity[Power],
        cosPhiRated: Double,
        nodalVoltage: ComparableQuantity[Dimensionless]
    ): ComparableQuantity[Power] => ComparableQuantity[Power] = {
      activePower: ComparableQuantity[Power] =>
        /* cosphi( P / P_N ) = cosphi( P / (S_N * cosphi_rated) ) */
        val pInPu =
          activePower
            .divide(sRated.multiply(cosPhiRated))
            .asType(classOf[Dimensionless])
        val instantCosPhi = cosPhi(pInPu)
        _cosPhiMultiplication(instantCosPhi.getValue.doubleValue, activePower)
    }
  }

}
