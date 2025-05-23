/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.asKiloWattHour
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  EnergyToSimona,
  PowerConversionSimona,
}
import squants.Power
import squants.energy.{Energy, KilowattHours}

import java.util.UUID

/** Wrapper for objects that extend [[EvModel]], which uses
  * [[javax.measure.Quantity]]. Since operations on javax/indriya quantities are
  * a bit slow, we lazily convert them to [[squants.Quantity]]. When
  * "unwrapping", we store back the only value that can have changed (while
  * considering immutability, i.e. when using [[copy]]), which is
  * [[storedEnergy]].
  *
  * @param storedEnergy
  *   Currently stored energy in the EV battery
  * @param original
  *   The wrapped [[EvModel]]
  */
final case class EvModelWrapper(
    storedEnergy: Energy,
    private val original: EvModel,
) {

  def uuid: UUID = original.getUuid
  def id: String = original.getId

  lazy val pRatedAc: Power = original.getPRatedAC.toSquants
  lazy val pRatedDc: Power = original.getPRatedDC.toSquants
  lazy val eStorage: Energy = original.getEStorage.toSquants

  def departureTick: Long = original.getDepartureTick

  /** Unwrapping the original [[EvModel]] while also updating the
    * [[storedEnergy]], which could have changed.
    *
    * @return
    *   The original [[EvModel]] with updated stored energy.
    */
  def unwrap(): EvModel =
    original.copyWith(
      storedEnergy.toKilowattHours.asKiloWattHour
    )

}

object EvModelWrapper {

  def apply(evModel: EvModel): EvModelWrapper =
    new EvModelWrapper(
      KilowattHours(
        evModel.getStoredEnergy.to(KILOWATTHOUR).getValue.doubleValue
      ),
      evModel,
    )

}
