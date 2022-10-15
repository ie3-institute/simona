/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  PrimaryDataWithApparentPower
}
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}

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
  * @tparam PD
  *   Primary data, that this asset does produce
  * @tparam MS
  *   Type of model state data
  */
abstract class SystemParticipant[
    CD <: CalcRelevantData,
    +PD <: PrimaryDataWithApparentPower[PD],
    MS <: ModelState
](
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double
) extends SystemComponent(uuid, id, operationInterval) {

  /** Maximum allowed apparent power output of this system participant. Used to
    * limit the available reactive power. Defaults to sRated. Should be
    * overwritten if the system participant's apparent power can be higher than
    * sRated.
    */
  protected val sMax: ComparableQuantity[Power] =
    sRated.to(PowerSystemUnits.KILOVOLTAMPERE)

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
      voltage: ComparableQuantity[Dimensionless],
      data: CD
  ): PD

  /** Calculate the apparent power behaviour based on the given data.
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
  protected def calculateApparentPower(
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      data: CD
  ): ApparentPower = {
    if (isInOperation(tick)) {
      val activePower = calculateActivePower(data).to(MEGAWATT)
      val reactivePower =
        calculateReactivePower(activePower, voltage).to(MEGAVAR)
      ApparentPower(activePower, reactivePower)
    } else {
      ApparentPower(
        Quantities.getQuantity(0d, MEGAWATT),
        Quantities.getQuantity(0d, MEGAVAR)
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
  protected def calculateActivePower(data: CD): ComparableQuantity[Power]

  /** @param data
    * @param lastState
    * @return
    *   flex options
    */
  def determineFlexOptions(
      data: CD,
      lastState: MS
  ): ProvideFlexOptions

  /** @param data
    * @param lastState
    * @param setPower
    *   power that has been set by EmAgent
    * @return
    *   updated relevant data and an indication at which circumstances flex
    *   options will change next
    */
  def handleControlledPowerChange(
      data: CD,
      lastState: MS,
      setPower: ComparableQuantity[Power]
  ): (MS, FlexChangeIndicator)

  /** Get a partial function, that transfers the current active into reactive
    * power based on the participants properties and the given nodal voltage
    *
    * @param nodalVoltage
    *   The currently given nodal voltage
    * @return
    *   A [[PartialFunction]] from [[ComparableQuantity]] of type [[Power]] to
    *   [[ComparableQuantity]] of type [[Power]]
    */
  def activeToReactivePowerFunc(
      nodalVoltage: ComparableQuantity[Dimensionless]
  ): ComparableQuantity[Power] => ComparableQuantity[Power] =
    qControl.activeToReactivePowerFunc(
      sRated.multiply(scalingFactor),
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
      activePower: ComparableQuantity[Power],
      voltage: ComparableQuantity[Dimensionless]
  ): ComparableQuantity[Power] = {
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
      activePower: ComparableQuantity[Power],
      reactivePower: ComparableQuantity[Power]
  ): ComparableQuantity[Power] = {
    {
      val apparentPower: ComparableQuantity[Power] = Quantities.getQuantity(
        Math
          .sqrt(
            Math.pow(activePower.to(KILOWATT).getValue.doubleValue, 2) + Math
              .pow(reactivePower.to(KILOVAR).getValue.doubleValue, 2)
          ),
        KILOVOLTAMPERE
      )

      if (apparentPower.isGreaterThan(sMax)) {
        logger.warn(
          s"The var characteristics \'$qControl\' of model \'$id\' ($uuid) imposes an apparent " +
            s"power (= $apparentPower) that exceeds " +
            s"rated apparent power specifications (= $sMax). " +
            s"Therefore, setting reactive power output to the to the upper limit " +
            s"in correspondence to the existing active power $activePower."
        )

        val powerSquaredDifference = Math
          .pow(sMax.to(MEGAVOLTAMPERE).getValue.doubleValue, 2) -
          Math.pow(activePower.to(MEGAWATT).getValue.doubleValue, 2)

        if (powerSquaredDifference < 0) {
          logger.warn(
            s"Difference between sMax and active power is negative when limiting reactive power. " +
              s"Set reactive power to 0!"
          )
          Quantities.getQuantity(0, MEGAVAR)
        } else {
          Quantities
            .getQuantity(
              Math.sqrt(powerSquaredDifference),
              MEGAVAR
            )
            .multiply(
              if (reactivePower.getValue.doubleValue < 0) -1 else 1
            ) // preserve the sign of reactive power
        }
      } else
        reactivePower
    }
  }

  def getUuid: UUID = this.uuid
}
