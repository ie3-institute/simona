/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import org.apache.kafka.common.serialization.{Deserializer, Serializer}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

/** As seen at
  * https://kafka-tutorials.confluent.io/produce-consume-lang/scala.html
  */
object ScalaReflectionSerde {

  def reflectionSerializer4S[T]: Serializer[T] =
    (topic: String, maybeData: T) => Option(maybeData)
      .map(data =>
        val outputStream = new ByteArrayOutputStream()
        val objectOutputStream = new ObjectOutputStream(outputStream)
        objectOutputStream.writeObject(data)
        objectOutputStream.close()
        outputStream.toByteArray
      )
      .getOrElse(Array.emptyByteArray)

  def reflectionDeserializer4S[T]: Deserializer[T] =
    (topic: String, maybeData: Array[Byte]) => Option(maybeData)
      .filter(_.nonEmpty)
      .map(data =>
        val inputStream = new ObjectInputStream(new ByteArrayInputStream(data))
        val value = inputStream.readObject() match {
          case t: T =>
            t
        }
        inputStream.close()
        value
      )
      .getOrElse(null.asInstanceOf[T])
}
