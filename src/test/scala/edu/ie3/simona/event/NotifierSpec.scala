/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import edu.ie3.datamodel.models.result.system.*
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.ConfigUtil.NotifierIdentifier.*
import edu.ie3.simona.util.EntityMapperUtil

import scala.language.postfixOps

class NotifierSpec extends UnitSpec {

  "The notifier object" should {
    "provide notifier to result entity mappings" in {
      val entityMapping = Map(
        PvPlant ->
          classOf[PvResult],
        Wec ->
          classOf[WecResult],
        Load ->
          classOf[LoadResult],
        FixedFeedIn ->
          classOf[FixedFeedInResult],
        BioMassPlant ->
          classOf[BmResult],
        Evcs ->
          classOf[EvcsResult],
        Storage ->
          classOf[StorageResult],
        Ev ->
          classOf[EvResult],
        Em ->
          classOf[EmResult],
      )
      // TODO: Grid results are not covered, yet.

      entityMapping.forall { case (emitter, entityClass) =>
        EntityMapperUtil.getResultEntityClass(emitter).equals(entityClass)
      } shouldBe true
    }
  }
}
