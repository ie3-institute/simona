/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data

import java.time.ZonedDateTime
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.value.{
  HeatAndPValue,
  HeatAndSValue,
  PValue,
  SValue,
  Value
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.util.quantities.interfaces.EnergyPrice

import javax.measure.quantity.Power
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

/** Trait to describe data structures, that are provided from the outside of a
  * [[edu.ie3.simona.model.participant.SystemParticipant]] model but not
  * necessarily from the outside of the simulation (but could be).
  */
sealed trait Data

object Data {

  /** Primary data are those, that are result of a model simulation. Mandatory
    * for grid interaction are at lease active and reactive power. Those data
    * structures here, also describe derivatives of this tuple, as those might
    * be delivered from the outside of the simulation and be extended by partial
    * model invocation. Anyway, primary data has to have at least active power
    * given
    */
  sealed trait PrimaryData extends Data {
    val p: ComparableQuantity[Power]

    def toApparentPower: ApparentPower
  }

  object PrimaryData {

    sealed trait EnrichableData[E <: PrimaryDataWithApparentPower[E]] {
      def add(q: ComparableQuantity[Power]): E
    }

    /** Denoting all primary data, that carry apparent power
      */
    sealed trait PrimaryDataWithApparentPower[
        +T <: PrimaryDataWithApparentPower[T]
    ] extends PrimaryData {
      val q: ComparableQuantity[Power]

      def withReactivePower(q: ComparableQuantity[Power]): T
    }

    /** Adding thermal power
      */
    sealed trait Heat {
      val qDot: ComparableQuantity[Power]
    }

    val ZERO_POWER: ApparentPower = ApparentPower(
      Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
      Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
    )

    /** Active power as participant simulation result
      *
      * @param p
      *   Active power
      */
    final case class ActivePower(override val p: ComparableQuantity[Power])
        extends PrimaryData
        with EnrichableData[ApparentPower] {
      override def toApparentPower: ApparentPower =
        ApparentPower(
          p,
          Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
        )

      override def add(q: ComparableQuantity[Power]): ApparentPower =
        ApparentPower(p, q)
    }

    /** Active and Reactive power as participant simulation result
      *
      * @param p
      *   Active power
      * @param q
      *   Reactive power
      */
    final case class ApparentPower(
        override val p: ComparableQuantity[Power],
        override val q: ComparableQuantity[Power]
    ) extends PrimaryDataWithApparentPower[ApparentPower] {
      override def toApparentPower: ApparentPower = this

      override def withReactivePower(
          q: ComparableQuantity[Power]
      ): ApparentPower = copy(q = q)
    }

    /** Active power and heat demand as participant simulation result
      *
      * @param p
      *   Active power
      * @param qDot
      *   Heat demand
      */
    final case class ActivePowerAndHeat(
        override val p: ComparableQuantity[Power],
        override val qDot: ComparableQuantity[Power]
    ) extends PrimaryData
        with Heat
        with EnrichableData[ApparentPowerAndHeat] {
      override def toApparentPower: ApparentPower =
        ApparentPower(
          p,
          Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
        )

      override def add(q: ComparableQuantity[Power]): ApparentPowerAndHeat =
        ApparentPowerAndHeat(p, q, qDot)
    }

    /** Apparent power and heat demand as participant simulation result
      *
      * @param p
      *   Active power
      * @param q
      *   Reactive power
      * @param qDot
      *   Heat demand
      */
    final case class ApparentPowerAndHeat(
        override val p: ComparableQuantity[Power],
        override val q: ComparableQuantity[Power],
        override val qDot: ComparableQuantity[Power]
    ) extends PrimaryDataWithApparentPower[ApparentPowerAndHeat]
        with Heat {
      override def toApparentPower: ApparentPower =
        ApparentPower(p, q)

      override def withReactivePower(
          q: ComparableQuantity[Power]
      ): ApparentPowerAndHeat = copy(q = q)
    }

    implicit class RichValue(private val value: Value) {
      def toPrimaryData: Try[PrimaryData] =
        value match {
          case hs: HeatAndSValue =>
            (hs.getP.toScala, hs.getQ.toScala, hs.getHeatDemand.toScala) match {
              case (Some(p), Some(q), Some(qDot)) =>
                Success(ApparentPowerAndHeat(p, q, qDot))
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$hs' to primary data."
                  )
                )
            }
          case s: SValue =>
            (s.getP.toScala, s.getQ.toScala) match {
              case (Some(p), Some(q)) => Success(ApparentPower(p, q))
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$s' to primary data."
                  )
                )
            }
          case hp: HeatAndPValue =>
            (hp.getP.toScala, hp.getHeatDemand.toScala) match {
              case (Some(p), Some(qDot)) => Success(ActivePowerAndHeat(p, qDot))
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$hp' to primary data."
                  )
                )
            }
          case p: PValue =>
            p.getP.toScala match {
              case Some(p) => Success(ActivePower(p))
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$p' to primary data."
                  )
                )
            }
          case _ =>
            Failure(
              new IllegalArgumentException(
                s"Cannot convert '$value' to primary data."
              )
            )
        }
    }
  }

  /** Trait to describe data structures of secondary data, that is used by
    * participant models to determine their actual interaction with the grid
    */
  trait SecondaryData extends Data
  object SecondaryData {
    final case class DateTime(dateTime: ZonedDateTime) extends SecondaryData
    final case class WholesalePrice(price: ComparableQuantity[EnergyPrice])
        extends SecondaryData
  }
}
