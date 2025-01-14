/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import scala.reflect.ClassTag
import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.TypeTag
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
    val clazz = ScalaClassLoader(getClass.getClassLoader)
      .tryToLoadClass(className)
      .getOrElse(
        throw new RuntimeException(
          s"Could not load class $className (It needs to be a fully-qualified class name)"
        )
      )
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    Try(
      mirror.reflectModule(mirror.moduleSymbol(clazz)).instance
    ).toOption
  }

  /** Determine the field and their value of the provided object instance
    *
    * @param a
    *   the object that should be processed
    * @param ct
    *   the class tag of the objects class
    * @tparam A
    *   type of the object
    * @return
    *   a map containing the field method, and it's value of the object instance
    */
  def classFieldToVal[A](a: A)(implicit
      ct: ClassTag[A]
  ): Map[universe.MethodSymbol, Any] = {
    val members = tt.tpe.members.collect {
      case m if m.isMethod && m.asMethod.isCaseAccessor => m.asMethod
    }
    members.map { member =>
      val memberValue = tt.mirror.reflect(a).reflectMethod(member)()
      member -> memberValue
    }.toMap
  }
}
