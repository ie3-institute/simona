/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{
  DefaultQuantities,
  Megavars,
  ReactivePower
}
import squants.Dimensionless
import squants.energy.{Kilowatts, Power}

import java.util.UUID

/** Common properties of mathematical models for system participants
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param scalingFactor
  *   Scaling the output of the system
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @tparam CD
  *   Type of data, that is needed for model calculation
  */
abstract class SystemParticipant[CD <: CalcRelevantData](
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double
) extends SystemComponent(uuid, id, operationInterval) {

  /** Maximum allowed apparent power output of this system participant. Used to
    * limit the available reactive power. Defaults to sRated. Should be
    * overwritten if the system participant's apparent power can be higher than
    * sRated.
    */
  protected val sMax: Power = sRated

  /** Calculate the power behaviour based on the given data.
    *
    * @param tick
    *   Regarded instant in simulation
    * @param voltage
    *   Nodal voltage magnitude
    * @param data
    *   Further needed, secondary data
    * @return
    *   A tuple of active and reactive power
    */
  def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      data: CD
  ): ApparentPower = {
    if (isInOperation(tick)) {
      val activePower = calculateActivePower(data)
      val reactivePower =
        calculateReactivePower(activePower, voltage)
      ApparentPower(activePower, reactivePower)
    } else {
      ApparentPower(
        DefaultQuantities.zeroMW,
        DefaultQuantities.zeroMVAr
      )
    }
  }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  protected def calculateActivePower(data: CD): Power

  /** Get a partial function, that transfers the current active into reactive
    * power based on the participants properties and the given nodal voltage
    *
    * @param nodalVoltage
    *   The currently given nodal voltage
    * @return
    *   A [[PartialFunction]] from [[Power]] to [[ReactivePower]]
    */
  def activeToReactivePowerFunc(
      nodalVoltage: Dimensionless
  ): Power => ReactivePower =
    qControl.activeToReactivePowerFunc(
      sRated,
      cosPhiRated,
      nodalVoltage
    )

  /** Calculate the reactive power of the model
    *
    * @param activePower
    *   Active power to use
    * @param voltage
    *   Voltage magnitude at connection point
    * @return
    *   Reactive power
    */
  def calculateReactivePower(
      activePower: Power,
      voltage: Dimensionless
  ): ReactivePower = {
    limitReactivePower(
      activePower,
      activeToReactivePowerFunc(voltage)(activePower)
    )
  }

  /** Check if the calculated apparent power respects model limits and return
    * adjusted reactive power value if necessary
    *
    * @param activePower
    *   calculated active power
    * @param reactivePower
    *   calculated reactive power, based on model var characteristics
    * @return
    *   reactivePower
    */
  private def limitReactivePower(
      activePower: Power,
      reactivePower: ReactivePower
  ): ReactivePower = {
    {
      val apparentPower: Power = Kilowatts(
        Math
          .sqrt(
            Math.pow(activePower.toKilowatts, 2) + Math
              .pow(reactivePower.toKilovars, 2)
          )
      )

      // tolerance for double inaccuracies
      val sMaxWithTolerance = sMax * 1.00001d

      if (apparentPower > sMaxWithTolerance) {
        logger.debug(
          s"The var characteristics \'$qControl\' of model \'$id\' ($uuid) imposes an apparent " +
            s"power (= $apparentPower) that exceeds " +
            s"rated apparent power specifications (= $sMax). " +
            s"Therefore, setting reactive power output to the to the upper limit " +
            s"in correspondence to the existing active power $activePower."
        )

        val powerSquaredDifference = Math.pow(sMax.toMegawatts, 2) -
          Math.pow(activePower.toMegawatts, 2)

        if (powerSquaredDifference < 0) {
          logger.warn(
            s"Active power of model exceeds sRated. Set reactive power to 0!"
          )
          Megavars(0d)
        } else {
          Megavars(
            Math.sqrt(powerSquaredDifference)
          ) * (if (reactivePower.toMegavars < 0) -1
               else 1) // preserve the sign of reactive power
        }
      } else
        reactivePower
    }
  }

  def getUuid: UUID = this.uuid
}
