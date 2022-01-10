/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import java.io.{
  ByteArrayOutputStream,
  File,
  FileOutputStream,
  NotSerializableException,
  ObjectOutputStream,
  PrintWriter
}

/** A tool to measure the size of objects and its fields recursively. Uses Java
  * serialization to measure size. Sizes are not accurate because of
  * serialization overhead.
  *
  * @author
  *   peter
  *
  * @param out
  *   print writer to log the results to
  */
class MeasureObjectSizes(out: PrintWriter) {

  private val stoplist = Set(
    "Integer",
    "String",
    "Boolean",
    "Double",
    "Long",
    "BigInteger",
    "BigDecimal",
    "Byte",
    "NumberQuantity",
    "Point",
    "CommonVoltageLevel",
    "LocalActorRef",
    "SimpleConfig",
    "NodeInput",
    "HashMap"
  )

  private val maxLevel = 3

  /** Log sizes of this object, the object's fields and the fields' fields etc.
    *
    * @param obj
    *   Current object
    * @param name
    *   Name of the current object (e.g. field name)
    * @param level
    *   Current recursion level (start with 0)
    */
  def measureRecursively(obj: Any, name: String, level: Int = 0): Unit = {
    val rec = Range(0, level).foldLeft("") { (str, _) =>
      str + "-"
    }

    try {
      val byteArray = new ByteArrayOutputStream(10000)
      val oos = new ObjectOutputStream(byteArray)
      oos.writeObject(obj)
      oos.close()

      out.println(
        s"$rec $name (${obj.getClass.getSimpleName}): ${byteArray.size()}b"
      )
      byteArray.size()
    } catch {
      case _: NotSerializableException =>
        out.println(
          s"$rec $name (${obj.getClass.getSimpleName}): Not serializable"
        )
        return
    }

    if (stoplist.contains(obj.getClass.getSimpleName) || level >= maxLevel)
      return

    measureFields(obj, obj.getClass, name, level)
  }

  /** Since only fields of current subclass are returned by
    * [[java.lang.Class]]#getDeclaredFields, we have to check superclasses too.
    *
    * @param obj
    *   Current object
    * @param clazz
    *   Class of which fields to get from
    * @param name
    *   Name of the current object (e.g. field name)
    * @param level
    *   Current recursion level
    */
  private def measureFields(
      obj: Any,
      clazz: Class[_],
      name: String,
      level: Int
  ): Unit = {
    val superclazz = clazz.getSuperclass

    if (superclazz.getSimpleName != "Object") {
      measureFields(obj, superclazz, name, level)
    }

    clazz.getDeclaredFields.foreach { field =>
      field.setAccessible(true)
      val childObj = field.get(obj)
      val name = field.getName
      measureRecursively(childObj, name, level + 1)
    }
  }

  def close(): Unit =
    out.close()
}

object MeasureObjectSizes {
  def apply(file: File): MeasureObjectSizes =
    new MeasureObjectSizes(
      new PrintWriter(
        new FileOutputStream(file)
      )
    )
}
