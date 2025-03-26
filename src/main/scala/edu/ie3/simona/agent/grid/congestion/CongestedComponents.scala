/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  LineModel,
  Transformer3wModel,
  TransformerModel,
  VoltageLimits,
}
import squants.electro.{ElectricPotential, Kilovolts}
import squants.energy.Power
import squants.{Amperes, ElectricCurrent}
import tech.units.indriya.unit.Units

import java.lang.Math.sqrt

case class CongestedComponents(
    voltages: Iterable[NodeResult],
    lines: Iterable[(LineModel, ElectricCurrent)],
    transformer2Ws: Iterable[(TransformerModel, Power)],
    transformer3Ws: Iterable[(Transformer3wModel, Power)],
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

          val vMag = nodeRes(
            transformer.lvNodeUuid
          ).getvMag().getValue.doubleValue() * vNom.toKilovolts

          // Units: kW -> A * kV
          transformer -> Amperes(
            sqrt(3.0) * res
              .getiBMag()
              .to(Units.AMPERE)
              .getValue
              .doubleValue()
          ) * Kilovolts(vMag)
        }
        .filter { case (transformer, result) => result > transformer.sRated }

    val transformer3w = gridComponents.transformers3w.map { transformer =>
      transformer.uuid -> transformer
    }.toMap
    val transformer3wCongestion =
      powerFlowResults.transformer3wResults
        .map { res =>
          val transformer = transformer3w(res.input)

          val vMag = nodeRes(
            transformer.lvNodeUuid
          ).getvMag().getValue.doubleValue() * vNom.toKilovolts

          transformer ->
            sqrt(
              3.0
            ) * res.currentMagnitude * Kilovolts(vMag)
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
