/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.data

import edu.ie3.datamodel.models.value._
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.interfaces.EnergyPrice
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{Kilovars, ReactivePower}
import squants.energy.{Kilowatts, Power}
import tech.units.indriya.ComparableQuantity

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
    val p: Power
    def toApparentPower: ApparentPower
  }

  sealed trait PrimaryDataMeta[T <: PrimaryData] {
    def zero: T

    def scale(data: T, factor: Double): T
  }

  object PrimaryData {

    sealed trait EnrichableData[E <: PrimaryDataWithApparentPower[E]] {
      def add(q: ReactivePower): E
    }

    /** Denoting all primary data, that carry apparent power
      */
    sealed trait PrimaryDataWithApparentPower[
        +T <: PrimaryDataWithApparentPower[T]
    ] extends PrimaryData {
      val q: ReactivePower

      def withReactivePower(q: ReactivePower): T
    }

    /** Adding thermal power
      */
    sealed trait Heat {
      val qDot: Power
    }

    val ZERO_POWER: ApparentPower = ApparentPower(zeroMW, zeroMVAr)

    /** Active power as participant simulation result
      *
      * @param p
      *   Active power
      */
    final case class ActivePower(override val p: Power)
        extends PrimaryData
        with EnrichableData[ApparentPower] {
      override def toApparentPower: ApparentPower =
        ApparentPower(
          p,
          zeroMVAr,
        )

      override def add(q: ReactivePower): ApparentPower =
        ApparentPower(p, q)
    }

    object ActivePowerMeta extends PrimaryDataMeta[ActivePower] {
      override def zero: ActivePower = ActivePower(zeroKW)

      override def scale(data: ActivePower, factor: Double): ActivePower =
        ActivePower(data.p * factor)
    }

    /** Active and Reactive power as participant simulation result
      *
      * @param p
      *   Active power
      * @param q
      *   Reactive power
      */
    final case class ApparentPower(
        override val p: Power,
        override val q: ReactivePower,
    ) extends PrimaryDataWithApparentPower[ApparentPower] {
      override def toApparentPower: ApparentPower = this

      override def withReactivePower(q: ReactivePower): ApparentPower =
        copy(q = q)
    }

    object ApparentPowerMeta extends PrimaryDataMeta[ApparentPower] {
      override def zero: ApparentPower = ApparentPower(zeroKW, zeroKVAr)

      override def scale(data: ApparentPower, factor: Double): ApparentPower =
        ApparentPower(data.p * factor, data.q * factor)
    }

    /** Active power and heat demand as participant simulation result
      *
      * @param p
      *   Active power
      * @param qDot
      *   Heat demand
      */
    final case class ActivePowerAndHeat(
        override val p: Power,
        override val qDot: Power,
    ) extends PrimaryData
        with Heat
        with EnrichableData[ApparentPowerAndHeat] {
      override def toApparentPower: ApparentPower =
        ApparentPower(
          p,
          zeroMVAr,
        )

      override def add(q: ReactivePower): ApparentPowerAndHeat =
        ApparentPowerAndHeat(p, q, qDot)
    }

    object ActivePowerAndHeatMeta extends PrimaryDataMeta[ActivePowerAndHeat] {
      override def zero: ActivePowerAndHeat = ActivePowerAndHeat(zeroKW, zeroKW)

      override def scale(
          data: ActivePowerAndHeat,
          factor: Double,
      ): ActivePowerAndHeat =
        ActivePowerAndHeat(data.p * factor, data.qDot * factor)
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
        override val p: Power,
        override val q: ReactivePower,
        override val qDot: Power,
    ) extends PrimaryDataWithApparentPower[ApparentPowerAndHeat]
        with Heat {
      override def toApparentPower: ApparentPower =
        ApparentPower(p, q)

      override def withReactivePower(q: ReactivePower): ApparentPowerAndHeat =
        copy(q = q)
    }

    object ApparentPowerAndHeatMeta
        extends PrimaryDataMeta[ApparentPowerAndHeat] {
      override def zero: ApparentPowerAndHeat =
        ApparentPowerAndHeat(zeroKW, zeroKVAr, zeroKW)

      override def scale(
          data: ApparentPowerAndHeat,
          factor: Double,
      ): ApparentPowerAndHeat =
        ApparentPowerAndHeat(
          data.p * factor,
          data.q * factor,
          data.qDot * factor,
        )
    }

    implicit class RichValue(private val value: Value) {
      def toPrimaryData: Try[PrimaryData] =
        value match {
          case hs: HeatAndSValue =>
            (hs.getP.toScala, hs.getQ.toScala, hs.getHeatDemand.toScala) match {
              case (Some(p), Some(q), Some(qDot)) =>
                Success(
                  ApparentPowerAndHeat(
                    Kilowatts(
                      p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
                    ),
                    Kilovars(
                      q.to(PowerSystemUnits.KILOVAR).getValue.doubleValue
                    ),
                    Kilowatts(
                      qDot.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
                    ),
                  )
                )
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$hs' to primary data."
                  )
                )
            }
          case s: SValue =>
            (s.getP.toScala, s.getQ.toScala) match {
              case (Some(p), Some(q)) =>
                Success(
                  ApparentPower(
                    Kilowatts(
                      p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
                    ),
                    Kilovars(
                      q.to(PowerSystemUnits.KILOVAR).getValue.doubleValue
                    ),
                  )
                )
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$s' to primary data."
                  )
                )
            }
          case hp: HeatAndPValue =>
            (hp.getP.toScala, hp.getHeatDemand.toScala) match {
              case (Some(p), Some(qDot)) =>
                Success(
                  ActivePowerAndHeat(
                    Kilowatts(
                      p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
                    ),
                    Kilowatts(
                      qDot.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
                    ),
                  )
                )
              case _ =>
                Failure(
                  new IllegalArgumentException(
                    s"Cannot convert '$hp' to primary data."
                  )
                )
            }
          case p: PValue =>
            p.getP.toScala match {
              case Some(p) =>
                Success(
                  ActivePower(
                    Kilowatts(
                      p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue
                    )
                  )
                )
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
    final case class WholesalePrice(price: ComparableQuantity[EnergyPrice])
        extends SecondaryData
  }
}
