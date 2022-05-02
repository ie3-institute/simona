/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.HEMSModel.HEMSRelevantData
import edu.ie3.simona.model.participant.PVModel.PVRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.interfaces.{Irradiance, Irradiation}
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import java.util.stream.IntStream
import javax.measure.Quantity
import javax.measure.quantity._
import scala.math._

final case class HEMSModel private (
) extends SystemParticipant[HEMSRelevantData](
    ) {


}

case object HEMSModel {

  /** Class that holds all relevant data for a pv model calculation
    *
    * @param dateTime
    *   date and time of the <b>ending</b> of time frame to calculate
    * @param weatherDataFrameLength
    *   the duration in ticks (= seconds) the provided irradiance is received by
    *   the pv panel
    * @param diffIrradiance
    *   diffuse solar irradiance
    * @param dirIrradiance
    *   direct solar irradiance
    */
  final case class HEMSRelevantData(
      // TODO: From PvModel, Check ant refactor
      dateTime: ZonedDateTime,
      weatherDataFrameLength: Long,
      diffIrradiance: ComparableQuantity[Irradiance],
      dirIrradiance: ComparableQuantity[Irradiance]
  ) extends CalcRelevantData

  def apply(
      // TODO: From PvModel, Check ant refactor
      inputModel: PvInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): PVModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    // moduleSurface and yieldSTC are left out for now
    val model = apply(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scalingFactor,
      QControl(inputModel.getqCharacteristics),
      inputModel.getsRated,
      inputModel.getCosPhiRated,
      inputModel.getNode.getGeoPosition.getY,
      inputModel.getNode.getGeoPosition.getX,
      inputModel.getAlbedo,
      inputModel.getEtaConv,
      inputModel.getAzimuth,
      inputModel.getElevationAngle
    )

    model.enable()

    model
  }

  // TODO: From PvModel, Check ant refactor
  /** Default factory method to create an PVModel instance. This constructor
    * ensures, that the angles passed in are converted to radian as required by
    * the model.
    *
    * @param uuid
    *   the unique id of the model
    * @param id
    *   the human readable id
    * @param operationInterval
    *   the operation interval of the model
    * @param scalingFactor
    *   the scaling factor of the power output
    * @param qControl
    *   the q control this model is using
    * @param sRated
    *   the rated apparent power of the model
    * @param cosPhiRated
    *   the rated cosine phi of the model
    * @param lat
    *   the latitude of the model
    * @param lon
    *   the longitude of the mode l
    * @param albedo
    *   the albedo of the model
    * @param etaConv
    *   the converter efficiency
    * @param alphaE
    *   the sun azimuth angle of the pv panel
    * @param gammaE
    *   the slope angle of the pv panel
    * @param moduleSurface
    *   the model surface size
    * @return
    */
  def apply(
      uuid: UUID,
      id: String,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      qControl: QControl,
      sRated: ComparableQuantity[Power],
      cosPhiRated: Double,
      lat: Double,
      lon: Double,
      albedo: Double,
      etaConv: ComparableQuantity[Dimensionless],
      alphaE: ComparableQuantity[Angle],
      gammaE: ComparableQuantity[Angle],
      moduleSurface: Quantity[Area] = Quantities.getQuantity(1d, SQUARE_METRE)
  ): PVModel = {
    val model = new PVModel(
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated,
      lat,
      lon,
      albedo,
      etaConv,
      alphaE.to(RADIAN),
      gammaE.to(RADIAN),
      moduleSurface
    )

    model.enable()

    model
  }

}
