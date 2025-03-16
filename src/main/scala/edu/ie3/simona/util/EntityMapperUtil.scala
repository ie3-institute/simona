/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system._
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  DomesticHotWaterStorageResult,
  ThermalHouseResult,
}
import edu.ie3.simona.util.ConfigUtil.NotifierIdentifier
import edu.ie3.simona.util.ConfigUtil.NotifierIdentifier._

object EntityMapperUtil {
  private val entityMapping
      : Map[NotifierIdentifier.Value, Class[_ <: ResultEntity]] = Map(
    PvPlant -> classOf[PvResult],
    Wec -> classOf[WecResult],
    Load -> classOf[LoadResult],
    FixedFeedIn -> classOf[FixedFeedInResult],
    BioMassPlant -> classOf[BmResult],
    Ev -> classOf[EvResult],
    Evcs -> classOf[EvcsResult],
    Storage -> classOf[StorageResult],
    Em -> classOf[EmResult],
    Hp -> classOf[HpResult],
    House -> classOf[ThermalHouseResult],
    CylindricalStorage -> classOf[CylindricalStorageResult],
    DomesticHotWaterStorage -> classOf[DomesticHotWaterStorageResult],
  )

  /** Get the classes of [[ResultEntity]], that are issued by the notifier, that
    * can be identified by the given id
    *
    * @param notifierId
    *   Identifier of a certain [[edu.ie3.simona.event.notifier.Notifier]]
    * @return
    *   An [[Option]] of classes that are childs of [[ResultEntity]]
    */
  def getResultEntityClass(
      notifierId: NotifierIdentifier.Value
  ): Class[_ <: ResultEntity] =
    entityMapping.getOrElse(
      notifierId,
      throw new NoSuchElementException(
        s"Cannot determine result entity class of notifier $notifierId"
      ),
    )
}
