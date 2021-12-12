/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

/** Trait for grouping entities, that denote the relevant data from model
  * calculations with system participants
  */
trait CalcRelevantData

object CalcRelevantData {

  /** Passed to model calculation classes for each participant when no secondary
    * data is required. For models, that require secondary data for calculation
    * specific classes are provided in corresponding model classes
    */
  case object FixedRelevantData extends CalcRelevantData

  /** A trait to group relevant data, as the load model is split up into two
    * partial models
    */
  trait LoadRelevantData extends CalcRelevantData

}
