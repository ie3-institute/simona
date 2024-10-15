/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import breeze.math.Complex
import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult,
}
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.agent.grid.SweepValueStore.SweepValueStoreData
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.Transformer3wModel.yij
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseA,
  PowerFlowCaseB,
  PowerFlowCaseC,
}
import edu.ie3.simona.model.grid._
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.QuantityUtil
import org.slf4j.Logger
import squants.space.Degrees
import squants.{Amperes, Angle, ElectricCurrent}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import scala.math._

/** Trait that holds methods to convert the results of a power flow calculation
  * to their corresponding [[edu.ie3.datamodel.models.result.ResultEntity]]
  */
private[grid] trait GridResultsSupport {

  /** Creates a tuple as [[PowerFlowResultEvent]] s entities based on the
    * provided grid data
    *
    * @param grid
    *   the grid model for which assets results should be created
    * @param sweepValueStore
    *   the value store with all power flow result values of the provided grid
    * @param timestamp
    *   the timestamp of the results
    * @return
    *   a tuple as [[PowerFlowResultEvent]] s with all results of the provided
    *   grid
    */
  def createResultModels(
      grid: GridModel,
      sweepValueStore: SweepValueStore,
  )(implicit timestamp: ZonedDateTime, tick: Long, nextTick: Long, log: Logger): PowerFlowResultEvent = {
    // no sanity check for duplicated uuid result data as we expect valid data at this point
    implicit val sweepValueStoreData: Map[UUID, SweepValueStoreData] =
      sweepValueStore.sweepData
        .map(sweepValueStoreData =>
          sweepValueStoreData.nodeUuid -> sweepValueStoreData
        )
        .toMap

    implicit val iNominal: ElectricCurrent =
      grid.mainRefSystem.nominalCurrent

    /* When creating node results, we have to consider two things:
     *   1) The result of a two winding transformer's hv node is calculated twice. If this grid contains the
     *      transformer, it is the lower voltage grid and there we don't want to announce the result, only in upper grid
     *   2) We are generally not interested in the result of a three winding transformer's internal node */
    val transformerNodesToIgnore = grid.gridComponents.transformers
      .map(_.hvNodeUuid) ++
      grid.gridComponents.transformers3w.map(_.nodeInternalUuid)
    PowerFlowResultEvent(
      sweepValueStoreData
        .filterNot { case (uuid, _) =>
          transformerNodesToIgnore.contains(uuid)
        }
        .values
        .map(calcNodeResult(_, timestamp)),
      grid.gridComponents.switches.map(calcSwitchResult(_, timestamp)),
      buildLineResults(grid.gridComponents.lines),
      buildTransformer2wResults(grid.gridComponents.transformers),
      buildTransformer3wResults(grid.gridComponents.transformers3w),
      tick,
      nextTick
    )
  }

  /** Build instances of [[LineResult]] based on a provided set of [[LineModel]]
    * and the corresponding sweep value data
    *
    * @param lines
    *   the set of lines which the result should be build for
    * @param sweepValueStoreData
    *   the value store with all power flow result values of the provided lines
    * @param iNominal
    *   the reference electric current of the grid
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   a set of [[LineResult]] s
    */
  private def buildLineResults(lines: Set[LineModel])(implicit
      sweepValueStoreData: Map[UUID, SweepValueStoreData],
      iNominal: squants.ElectricCurrent,
      timestamp: ZonedDateTime,
      log: Logger,
  ): Set[LineResult] = {
    lines.flatMap(lineModel => {
      sweepValueStoreData
        .get(lineModel.nodeAUuid)
        .zip(sweepValueStoreData.get(lineModel.nodeBUuid)) match {
        case Some((nodeAStateData, nodeBStateData)) =>
          Some(
            calcLineResult(
              lineModel,
              nodeAStateData.stateData,
              nodeBStateData.stateData,
              iNominal,
              timestamp,
            )
          )
        case None =>
          log.warn(
            "Cannot find power flow result data for line {} with nodeA {} and nodeB {}",
            lineModel.uuid,
            lineModel.nodeAUuid,
            lineModel.nodeBUuid,
          )
          None
      }
    })
  }

  /** Build instances of [[Transformer2WResult]] based on a provided set of
    * [[TransformerModel]] and the corresponding sweep value data
    *
    * @param transformers
    *   the set of transformers which the result should be build for
    * @param sweepValueStoreData
    *   the value store with all power flow result values of the provided
    *   transformers
    * @param iNominal
    *   the reference electric current of the grid
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   a set of [[Transformer2WResult]] s
    */
  private def buildTransformer2wResults(transformers: Set[TransformerModel])(
      implicit
      sweepValueStoreData: Map[UUID, SweepValueStoreData],
      iNominal: ElectricCurrent,
      timestamp: ZonedDateTime,
      log: Logger,
  ): Set[Transformer2WResult] = {
    transformers.flatMap(trafo2w => {
      sweepValueStoreData
        .get(trafo2w.hvNodeUuid)
        .zip(sweepValueStoreData.get(trafo2w.lvNodeUuid)) match {
        case Some((hvNodeStateData, lvNodeStateData)) =>
          Some(
            calcTransformer2wResult(
              trafo2w,
              hvNodeStateData.stateData,
              lvNodeStateData.stateData,
              iNominal,
              timestamp,
            )
          )
        case None =>
          log.warn(
            "Cannot find power flow result data for transformer2w {} with hvNode {} and lvNode {}",
            trafo2w.uuid,
            trafo2w.hvNodeUuid,
            trafo2w.lvNodeUuid,
          )
          None
      }
    })
  }

  /** Build instances of [[Transformer3WResult]] based on a provided set of
    * [[Transformer3wModel]] and the corresponding sweep value data
    *
    * @param transformers3w
    *   the set of 3 winding transformers which the result should be build for
    * @param sweepValueStoreData
    *   the value store with all power flow result values of the provided 3
    *   winding transformers
    * @param iNominal
    *   the reference electric current of the grid
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   a set of [[PartialTransformer3wResult]] s
    */
  def buildTransformer3wResults(transformers3w: Set[Transformer3wModel])(
      implicit
      sweepValueStoreData: Map[UUID, SweepValueStoreData],
      iNominal: ElectricCurrent,
      timestamp: ZonedDateTime,
      log: Logger,
  ): Set[PartialTransformer3wResult] = transformers3w.flatMap { trafo3w =>
    {
      (trafo3w.powerFlowCase match {
        case PowerFlowCaseA =>
          sweepValueStoreData
            .get(trafo3w.hvNodeUuid)
            .zip(sweepValueStoreData.get(trafo3w.nodeInternalUuid))
        case PowerFlowCaseB =>
          sweepValueStoreData
            .get(trafo3w.mvNodeUuid)
            .zip(sweepValueStoreData.get(trafo3w.nodeInternalUuid))
        case PowerFlowCaseC =>
          sweepValueStoreData
            .get(trafo3w.lvNodeUuid)
            .zip(sweepValueStoreData.get(trafo3w.nodeInternalUuid))
      }) match {
        case Some((upperNodeStateData, internalNodeStateData)) =>
          Some(
            calcTransformer3wResult(
              trafo3w,
              upperNodeStateData.stateData,
              internalNodeStateData.stateData,
              iNominal,
              timestamp,
            )
          )
        case None =>
          log.warn(
            s"Cannot find power flow result data for transformer3w {} with nodeHv {}, nodeMv {}, nodeLv {} and internalNode ${trafo3w.nodeInternalUuid}",
            trafo3w.uuid,
            trafo3w.hvNodeUuid,
            trafo3w.mvNodeUuid,
            trafo3w.lvNodeUuid,
          )
          None
      }
    }
  }

  /** Creates an instance of [[NodeResult]] based on the provided
    * [[SweepValueStoreData]]
    *
    * @param sweepValueStoreData
    *   the sweep value store with the node results
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   instance of [[NodeResult]] based on the provided data
    */
  protected def calcNodeResult(
      sweepValueStoreData: SweepValueStoreData,
      timestamp: ZonedDateTime,
  ): NodeResult = {

    val nodeStateData = sweepValueStoreData.stateData
    val vMag = nodeStateData.voltage.abs
    val vAng = asin(nodeStateData.voltage.imag / vMag).toDegrees

    new NodeResult(
      timestamp,
      sweepValueStoreData.nodeUuid,
      Quantities.getQuantity(vMag, PowerSystemUnits.PU),
      Quantities.getQuantity(vAng, PowerSystemUnits.DEGREE_GEOM),
    )
  }

  /** Creates an instance of [[SwitchResult]] based on the provided
    * [[SwitchModel]]
    *
    * @param switchModel
    *   the switch model that should be processed
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   instance of [[SwitchResult]] based on the provided data
    */
  protected def calcSwitchResult(
      switchModel: SwitchModel,
      timestamp: ZonedDateTime,
  ): SwitchResult = {
    /* can be adapted when https://github.com/ie3-institute/PowerSystemDataModel/issues/151 has been resolved */
    new SwitchResult(
      timestamp,
      switchModel.uuid,
      switchModel.isClosed,
    )
  }

  /** Creates an instance of [[LineResult]] based on the provided grid and power
    * flow result data
    *
    * @param line
    *   the instance of the [[LineModel]] that should be processed
    * @param nodeAStateData
    *   the power flow result state data of nodeA of the line
    * @param nodeBStateData
    *   the power flow result state data of nodeB of the line
    * @param iNominal
    *   the reference electric current of the grid
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   instance of [[LineResult]] based on the provided data
    */
  protected def calcLineResult(
      line: LineModel,
      nodeAStateData: StateData,
      nodeBStateData: StateData,
      iNominal: ElectricCurrent,
      timestamp: ZonedDateTime,
  ): LineResult = {

    if (line.isInOperation) {
      val yij = new Complex(
        line.gij().value.doubleValue,
        line.bij().value.doubleValue,
      )
      val y0 = new Complex(
        line.g0().value.doubleValue,
        line.b0().value.doubleValue,
      )

      val (iAComplexPu, iBComplexPu) =
        iIJComplexPu(nodeAStateData.voltage, nodeBStateData.voltage, yij, y0)

      val (iAMag, iAAng) = iMagAndAngle(iAComplexPu, iNominal)
      val (iBMag, iBAng) = iMagAndAngle(iBComplexPu, iNominal)

      new LineResult(
        timestamp,
        line.uuid,
        Quantities.getQuantity(iAMag.toAmperes, Units.AMPERE),
        Quantities.getQuantity(iAAng.toDegrees, PowerSystemUnits.DEGREE_GEOM),
        Quantities.getQuantity(iBMag.toAmperes, Units.AMPERE),
        Quantities.getQuantity(iBAng.toDegrees, PowerSystemUnits.DEGREE_GEOM),
      )
    } else {
      new LineResult(
        timestamp,
        line.uuid,
        QuantityUtil.zeroCompQuantity(Units.AMPERE),
        QuantityUtil.zeroCompQuantity(PowerSystemUnits.DEGREE_GEOM),
        QuantityUtil.zeroCompQuantity(Units.AMPERE),
        QuantityUtil.zeroCompQuantity(PowerSystemUnits.DEGREE_GEOM),
      )
    }
  }

  /** Creates an instance of [[Transformer2WResult]] based on the provided grid
    * and power flow result data
    *
    * @param trafo2w
    *   the instance of the 2 winding transformer that should be processed
    * @param hvNodeStateData
    *   the power flow result state data of the higher voltage node of the
    *   transformer
    * @param lvNodeStateData
    *   the power flow result state data of the lower voltage node of the
    *   transformer
    * @param iNominal
    *   the reference electric current of the grid
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   instance of [[Transformer2WResult]] based on the provided data
    */
  protected def calcTransformer2wResult(
      trafo2w: TransformerModel,
      hvNodeStateData: StateData,
      lvNodeStateData: StateData,
      iNominal: ElectricCurrent,
      timestamp: ZonedDateTime,
  ): Transformer2WResult = {
    if (trafo2w.isInOperation) {
      val (yab, yaa, ybb) = (
        TransformerModel.yij(trafo2w),
        TransformerModel.y0(trafo2w, ConnectorPort.A),
        TransformerModel.y0(trafo2w, ConnectorPort.B),
      )

      val voltRatioNominal = trafo2w.voltRatioNominal

      val (iAComplexPu, iBComplexPu) = iIJComplexPu(
        hvNodeStateData.voltage,
        lvNodeStateData.voltage,
        yab,
        yaa,
        Some(ybb),
      )

      /* Transfer port current A to high voltage level */
      val (iAMag, iAAng) =
        iMagAndAngle(iAComplexPu, iNominal / voltRatioNominal.toDouble)
      val (iBMag, iBAng) = iMagAndAngle(iBComplexPu, iNominal)

      new Transformer2WResult(
        timestamp,
        trafo2w.uuid,
        Quantities.getQuantity(iAMag.toAmperes, Units.AMPERE),
        Quantities.getQuantity(iAAng.toDegrees, PowerSystemUnits.DEGREE_GEOM),
        Quantities.getQuantity(iBMag.toAmperes, Units.AMPERE),
        Quantities.getQuantity(iBAng.toDegrees, PowerSystemUnits.DEGREE_GEOM),
        trafo2w.currentTapPos,
      )
    } else {

      new Transformer2WResult(
        timestamp,
        trafo2w.uuid,
        QuantityUtil.zeroCompQuantity(Units.AMPERE),
        QuantityUtil.zeroCompQuantity(PowerSystemUnits.DEGREE_GEOM),
        QuantityUtil.zeroCompQuantity(Units.AMPERE),
        QuantityUtil.zeroCompQuantity(PowerSystemUnits.DEGREE_GEOM),
        trafo2w.currentTapPos,
      )
    }
  }

  /** Creates an instance of [[Transformer3WResult]] based on the provided grid
    * and power flow result data
    *
    * @param trafo3w
    *   the instance of the 3 winding transformer that should be processed
    * @param nodeStateData
    *   the power flow case dependant power flow result state data (case A =
    *   nodeA, case B = nodeB, case C = nodeC)
    * @param internalNodeStateData
    *   the internal node power flow result state data
    * @param iNominal
    *   the reference electric current of the grid
    * @param timestamp
    *   the timestamp of the result
    * @return
    *   instance of [[PartialTransformer3wResult]] based on the provided data
    */
  protected def calcTransformer3wResult(
      trafo3w: Transformer3wModel,
      nodeStateData: StateData,
      internalNodeStateData: StateData,
      iNominal: ElectricCurrent,
      timestamp: ZonedDateTime,
  ): PartialTransformer3wResult = {
    val (_, iComplexPu) = iIJComplexPu(
      internalNodeStateData.voltage,
      nodeStateData.voltage,
      yij(trafo3w),
      Transformer3wModel.y0(
        trafo3w,
        trafo3w.powerFlowCase match {
          case PowerFlowCaseA => Transformer3wModel.Transformer3wPort.A
          case PowerFlowCaseB => Transformer3wModel.Transformer3wPort.B
          case PowerFlowCaseC => Transformer3wModel.Transformer3wPort.C
        },
      ),
      None,
    )

    val (iMag, iAng) = iMagAndAngle(iComplexPu, iNominal)

    trafo3w.powerFlowCase match {
      case Transformer3wPowerFlowCase.PowerFlowCaseA =>
        PartialTransformer3wResult.PortA(
          timestamp,
          trafo3w.uuid,
          iMag,
          iAng,
          trafo3w.currentTapPos,
        )
      case Transformer3wPowerFlowCase.PowerFlowCaseB =>
        PartialTransformer3wResult.PortB(
          timestamp,
          trafo3w.uuid,
          iMag,
          iAng,
        )
      case Transformer3wPowerFlowCase.PowerFlowCaseC =>
        PartialTransformer3wResult.PortC(
          timestamp,
          trafo3w.uuid,
          iMag,
          iAng,
        )
    }
  }

  /** Calculate the current magnitude and the current angle in physical units
    * based on a provided electric current in p.u. and the nominal referenced
    * electric current. The arctangent "only" calculates the angle between the
    * complex current and it's real part. This means, that i = (i_real, i_imag)
    * and i' = (-i_real, -i_imag) will lead to the same angle. However, for
    * power system simulation, the absolute orientation in the complex plane
    * with regard to the positive real axis is of interest. Therefore,
    * additional 180 degrees are added, if the real part of the current is
    * negative.
    *
    * @param iPu
    *   the electric current in p.u.
    * @param iNominal
    *   the reference electric current of the provided electric current in p.u.
    * @return
    *   the electric current magnitude and angle in physical units
    */
  private def iMagAndAngle(
      iPu: Complex,
      iNominal: ElectricCurrent,
  ): (ElectricCurrent, Angle) =
    (
      Amperes(iNominal.toAmperes * iPu.abs),
      complexToAngle(iPu),
    )

  /** Calculate the angle of the complex value given. The angle has the proper
    * orientation on the complex plane.
    *
    * @param cplx
    *   The complex value
    * @return
    *   The angle of the complex value
    */
  private def complexToAngle(cplx: Complex): Angle =
    cplx match {
      case Complex(0d, 0d) =>
        /* The complex value has no magnitude, therefore define the angle to zero */
        Degrees(0d)
      case Complex(0d, imag) =>
        /* There is only an imaginary part:
        Angle can be 90 or 270 degrees, depending on sign of the imaginary part */
        angleOffsetCorrection(
          Degrees(90d),
          imag,
        )
      case Complex(real, imag) =>
        /* Both real and imaginary parts are != 0. This means that the angle
         * related to the positive real axis is to be determined. To do this,
         * atan can be used to calculate an angle between -90 and 90 degrees.
         * To calculate the real angle (between -180 and 180 degrees) with
         * respect to the real axis, 180 degrees must be added if the real
         * part is negative. */
        val baseAngle = atan(imag / real).toDegrees
        angleOffsetCorrection(
          Degrees(baseAngle),
          real,
        )
    }

  /** Correct the offset of an angle dependent on the direction. If the
    * direction is negative, 180 degrees are added
    */
  private def angleOffsetCorrection(
      angle: Angle,
      dir: Double,
  ): Angle =
    if (dir < 0)
      angle + Degrees(180d)
    else
      angle

  /** Calculates the electric current of a two-port element @ port i (=A) and j
    * (=B) based on the provided voltages @ each port and the corresponding
    * admittances. All values in p.u.
    *
    * Applied formula: Ii = (ui-uj) * yij + ui * yii Ij = (uj-ui) * yij + uj *
    * yjj (transformers) or yii (lines)
    *
    * @param uiPu
    *   the voltage magnitude @ port i (=A) in p.u.
    * @param ujPu
    *   the voltage magnitude @ port j (=B) in p.u.
    * @param yij
    *   the complex admittance between port i and j in p.u.
    * @param y0i
    *   the phase-to-ground admittance @ port i in p.u.
    * @param y0j
    *   the optional phase-to-ground admittance @ port j in p.u.
    * @return
    *   the electric current @ port i (=A) and j (=B) in p.u.
    */
  private def iIJComplexPu(
      uiPu: Complex,
      ujPu: Complex,
      yij: Complex,
      y0i: Complex,
      y0j: Option[Complex] = None,
  ): (Complex, Complex) = {
    (
      (uiPu - ujPu) * yij + (uiPu * y0i),
      (ujPu - uiPu) * yij + (ujPu * y0j.getOrElse(y0i)),
    )
  }

}

object GridResultsSupport {

  /** Denoting a partial transformer result, that is calculated for only one of
    * the three available ports
    */
  sealed trait PartialTransformer3wResult {
    val time: ZonedDateTime
    val input: UUID
    protected val currentMagnitude: ElectricCurrent
    protected val currentAngle: Angle
  }

  object PartialTransformer3wResult {

    /** Partial result for the port at the high voltage side
      *
      * @param time
      *   Wall clock time, the result does belong to
      * @param input
      *   Unique identifier of the input model
      * @param currentMagnitude
      *   Magnitude of the port current
      * @param currentAngle
      *   Angle of the port current
      * @param tapPos
      *   Current position of tap changer
      */
    final case class PortA(
        override val time: ZonedDateTime,
        override val input: UUID,
        override val currentMagnitude: ElectricCurrent,
        override val currentAngle: Angle,
        tapPos: Int,
    ) extends PartialTransformer3wResult

    /** Partial result for the port at the medium voltage side
      *
      * @param time
      *   Wall clock time, the result does belong to
      * @param input
      *   Unique identifier of the input model
      * @param currentMagnitude
      *   Magnitude of the port current
      * @param currentAngle
      *   Angle of the port current
      */
    final case class PortB(
        override val time: ZonedDateTime,
        override val input: UUID,
        override val currentMagnitude: ElectricCurrent,
        override val currentAngle: Angle,
    ) extends PartialTransformer3wResult

    /** Partial result for the port at the low voltage side
      *
      * @param time
      *   Wall clock time, the result does belong to
      * @param input
      *   Unique identifier of the input model
      * @param currentMagnitude
      *   Magnitude of the port current
      * @param currentAngle
      *   Angle of the port current
      */
    final case class PortC(
        override val time: ZonedDateTime,
        override val input: UUID,
        override val currentMagnitude: ElectricCurrent,
        override val currentAngle: Angle,
    ) extends PartialTransformer3wResult
  }
}
