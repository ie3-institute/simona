/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import scala.quoted.*

import scala.reflect.ClassTag
import scala.util.Try

/** Function collection for reflection tasks
  */
object ReflectionTools {

  def identifyValidCompanions(classNames: Iterable[String]): Iterable[Any] = {
    classNames.flatMap(name => resolveClassNameToCompanion(name)).collect {
      case companion: Any => companion
    }
  }

  def resolveClassNameToCompanion(className: String): Option[Any] = {
    Try(Class.forName(className, false, getClass.getClassLoader)).toOption
  }
}
