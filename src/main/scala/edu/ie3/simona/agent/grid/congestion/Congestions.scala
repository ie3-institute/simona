/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.VoltageLimits
import squants.electro.ElectricPotential

import java.lang.Math.sqrt

/** Case class that contains information about congestions in a subgrid.
  *
  * @param voltageCongestions
  *   true if the lower or upper voltage limit is violated
  * @param lineCongestions
  *   true if there is a line congestion
  * @param transformerCongestions
  *   true if there is a transformer congestion
  */
final case class Congestions(
    voltageCongestions: Boolean,
    lineCongestions: Boolean,
    transformerCongestions: Boolean,
) {

  /** Returns true if any congestion occurred.
    */
  def any: Boolean =
    voltageCongestions || lineCongestions || transformerCongestions

  /** Returns true if there is either a line or transformer congestion
    */
  def assetCongestion: Boolean = lineCongestions || transformerCongestions

  /** Method for combining multiple [[Congestions]].
    * @param options
    *   that should be combined with the own options
    * @return
    *   a new [[Congestions]]
    */
  def combine(options: Iterable[Congestions]): Congestions =
    Congestions(
      voltageCongestions || options.exists(_.voltageCongestions),
      lineCongestions || options.exists(_.lineCongestions),
      transformerCongestions || options.exists(_.transformerCongestions),
    )
}

object Congestions {

  /** Method for finding congestions within the given power flow results.
    * @param powerFlowResults
    *   of the last simulation
    * @param gridComponents
    *   all [[GridComponents]]
    * @param voltageLimits
    *   the voltage limits of the grid
    * @param vNom
    *   the nominal voltage to calculate the transformer power
    * @param subnetNo
    *   number of the subgrid
    * @return
    *   a new [[Congestions]]
    */
  def apply(
      powerFlowResults: PowerFlowResultEvent,
      gridComponents: GridComponents,
      voltageLimits: VoltageLimits,
      vNom: ElectricPotential,
      subnetNo: Int,
  ): Congestions = {
    val nodeRes =
      powerFlowResults.nodeResults.map(res => res.getInputModel -> res).toMap

    // filter nodes in subnet
    val nodesInSubnet =
      gridComponents.nodes.filter(_.subnet == subnetNo).map(_.uuid)

    // checking for voltage congestions
    val voltageCongestion = nodeRes.values
      .filter(res => nodesInSubnet.contains(res.getInputModel))
      .exists { res =>
        !voltageLimits.isInLimits(res.getvMag())
      }

    // checking for line congestions
    val linesLimits = gridComponents.lines.map { line =>
      line.uuid -> line
    }.toMap
    val lineCongestion = powerFlowResults.lineResults.exists { res =>
      val iA = res.getiAMag().getValue.doubleValue()
      val iB = res.getiBMag().getValue.doubleValue()
      val iNom = linesLimits(res.getInputModel).iNom.value

      iA > iNom || iB > iNom
    }

    // checking for transformer congestions
    val transformer2w = gridComponents.transformers.map { transformer =>
      transformer.uuid -> transformer
    }.toMap
    val transformer2wCongestion =
      powerFlowResults.transformer2wResults.exists { res =>
        val transformer = transformer2w(res.getInputModel)

        val vMag = nodeRes(
          transformer.lvNodeUuid
        ).getvMag().getValue.doubleValue() * vNom.toKilovolts

        sqrt(3.0) * res
          .getiBMag()
          .getValue
          .doubleValue() * vMag > transformer.sRated.toKilowatts
      }

    val transformer3w = gridComponents.transformers3w.map { transformer =>
      transformer.uuid -> transformer
    }.toMap
    val transformer3wCongestion =
      powerFlowResults.transformer3wResults.exists { res =>
        val transformer = transformer3w(res.input)

        val vMag = nodeRes(
          transformer.lvNodeUuid
        ).getvMag().getValue.doubleValue() * vNom.toKilovolts

        sqrt(
          3.0
        ) * res.currentMagnitude.value * vMag > transformer.sRated.toKilowatts
      }

    Congestions(
      voltageCongestion,
      lineCongestion,
      transformer2wCongestion || transformer3wCongestion,
    )
  }
}
