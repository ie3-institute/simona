/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

/** Case class that contains information about congestions in a subgrid.
  *
  * @param voltageCongestions
  *   True if the lower or upper voltage limit is violated
  * @param lineCongestions
  *   True if there is a line congestion
  * @param transformerCongestions
  *   True if there is a transformer congestion
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
    *   That should be combined with the own options
    * @return
    *   A new [[Congestions]]
    */
  def combine(options: Iterable[Congestions]): Congestions =
    Congestions(
      voltageCongestions || options.exists(_.voltageCongestions),
      lineCongestions || options.exists(_.lineCongestions),
      transformerCongestions || options.exists(_.transformerCongestions),
    )
}

object Congestions {

  /** Method to build a [[Congestions]] from the information about congested
    * elements.
    *
    * @param congestedComponents
    *   All components that are congested.
    * @return
    *   A new [[Congestions]].
    */
  def apply(
      congestedComponents: CongestedComponents
  ): Congestions =
    Congestions(
      congestedComponents.voltages.nonEmpty,
      congestedComponents.lines.nonEmpty,
      congestedComponents.transformer2Ws.nonEmpty || congestedComponents.transformer3Ws.nonEmpty,
    )
}
