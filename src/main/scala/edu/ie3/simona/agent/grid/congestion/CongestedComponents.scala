/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult.{
  PortA,
  PortB,
  PortC,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  LineModel,
  Transformer3wModel,
  TransformerModel,
  VoltageLimits,
}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  CurrentToSimona,
  DimensionlessToSimona,
}
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.electro.{ElectricPotential, Kilovolts}
import squants.{Amperes, ElectricCurrent}
import tech.units.indriya.unit.Units

import java.lang.Math.sqrt

case class CongestedComponents(
    voltages: Iterable[NodeResult],
    lines: Iterable[(LineModel, ElectricCurrent)],
    transformer2Ws: Iterable[(TransformerModel, ApparentPower)],
    transformer3Ws: Iterable[(Transformer3wModel, ApparentPower)],
)

object CongestedComponents {

  /** Method for finding congestions within the given power flow results.
    * @param powerFlowResults
    *   Of the last simulation.
    * @param gridComponents
    *   All [[GridComponents]].
    * @param voltageLimits
    *   The voltage limits of the grid.
    * @param vNom
    *   The nominal voltage to calculate the transformer power.
    * @param subnetNo
    *   Number of the subgrid.
    * @return
    *   A new [[CongestedComponents]].
    */
  def apply(
      powerFlowResults: PowerFlowResultEvent,
      gridComponents: GridComponents,
      voltageLimits: VoltageLimits,
      vNom: ElectricPotential,
      subnetNo: Int,
  ): CongestedComponents = {
    val nodeRes =
      powerFlowResults.nodeResults.map(res => res.getInputModel -> res).toMap

    // filter nodes in subnet
    val nodesInSubnet =
      gridComponents.nodes.filter(_.subnet == subnetNo).map(_.uuid)

    // checking for voltage congestions
    val voltageCongestion = nodeRes.values
      .filter(res => nodesInSubnet.contains(res.getInputModel))
      .filter { res =>
        !voltageLimits.isInLimits(res.getvMag())
      }

    // checking for line congestions
    val linesLimits = gridComponents.lines.map { line =>
      line.uuid -> line
    }.toMap
    val lineCongestion = powerFlowResults.lineResults
      .map { res =>
        val iA = Amperes(res.getiAMag().to(Units.AMPERE).getValue.doubleValue())
        val iB = Amperes(res.getiBMag().to(Units.AMPERE).getValue.doubleValue())
        val line = linesLimits(res.getInputModel)

        // Units: A -> (A, A)

        val current = if (iA > iB) {
          iA
        } else iB

        line -> current
      }
      .filter { case (line, res) => res > line.iNom }

    // checking for transformer congestions
    val transformer2w = gridComponents.transformers.map { transformer =>
      transformer.uuid -> transformer
    }.toMap
    val transformer2wCongestion =
      powerFlowResults.transformer2wResults
        .map { res =>
          val transformer = transformer2w(res.getInputModel)

          val vMag =
            vNom * nodeRes(transformer.lvNodeUuid).getvMag.toSquants.toEach

          val power = sqrt(3.0) * res.getiBMag.toSquants * vMag
          transformer -> Kilovoltamperes(power.toKilowatts)
        }
        .filter { case (transformer, result) => result > transformer.sRated }

    val transformer3w = gridComponents.transformers3w.map { transformer =>
      transformer.uuid -> transformer
    }.toMap
    val transformer3wCongestion =
      powerFlowResults.transformer3wResults
        .map { res =>
          val transformer = transformer3w(res.input)

          val nodeUuid = res match {
            case _: PortA =>
              transformer.hvNodeUuid
            case _: PortB =>
              transformer.mvNodeUuid
            case _: PortC =>
              transformer.lvNodeUuid
          }

          val vMag = vNom * nodeRes(nodeUuid).getvMag.toSquants.toEach

          val power = sqrt(3.0) * res.currentMagnitude * vMag
          transformer -> Kilovoltamperes(power.toKilowatts)
        }
        .filter { case (transformer, result) => result > transformer.sRated }

    CongestedComponents(
      voltageCongestion,
      lineCongestion,
      transformer2wCongestion,
      transformer3wCongestion,
    )
  }

  def empty: CongestedComponents = CongestedComponents(
    Iterable.empty,
    Iterable.empty,
    Iterable.empty,
    Iterable.empty,
  )

}
