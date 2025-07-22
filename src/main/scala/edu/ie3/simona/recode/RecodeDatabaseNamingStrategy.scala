/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.recode

import edu.ie3.datamodel.io.naming.EntityPersistenceNamingStrategy
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}

import java.util.Optional

class RecodeDatabaseNamingStrategy extends EntityPersistenceNamingStrategy {

  override def getResultEntityName(
      resultEntityClass: Class[? <: ResultEntity]
  ): Optional[String] = {
    val NodeRes = classOf[NodeResult]

    resultEntityClass match {
      case NodeRes =>
        Optional.of("bus")
      case _ =>
        super.getResultEntityName(resultEntityClass)
    }
  }
}
