/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import com.sksamuel.avro4s.*
import edu.ie3.simona.io.result.plain.PlainResult
import io.confluent.kafka.streams.serdes.avro.{
  GenericAvroDeserializer,
  GenericAvroSerializer,
}
import org.apache.avro.Schema
import org.apache.kafka.common.serialization.{Deserializer, Serializer}

import java.util.UUID

/** As seen at
  * https://kafka-tutorials.confluent.io/produce-consume-lang/scala.html
  */
object ScalaReflectionSerde {

  def genericPlainResultJsonSerializer[P <: PlainResult](
      modelConfig: Map[UUID, Map[String, UUID]]
  ): Serializer[P] = (topic: String, data: P) => {
    val inputModel = data.inputModel

    if modelConfig.isEmpty then {
      val builder = new StringBuilder(
        s"simRunId: \"${data.simRunId}\", inputModel: \"$inputModel\""
      )

      data.asMap.foreach { case (key, value) =>
        builder.append(s", $key: \"$value\"")
      }

      builder.toString.getBytes

    } else {
      modelConfig
        .get(inputModel)
        .map { config =>
          val builder = new StringBuilder(
            s"simRunId: \"${data.simRunId}\", inputModel: \"$inputModel\""
          )

          data.asMap.filter(m => config.contains(m._1)).foreach {
            case (key, value) =>
              val mRID = config(key)
              builder.append(s", $mRID: \"$value\"")
          }

          builder.toString.getBytes
        }
        .getOrElse(Array.emptyByteArray)
    }
  }

  def reflectionSerializer4S[T >: Null: SchemaFor: Encoder]: Serializer[T] =
    new Serializer[T] {
      val inner = new GenericAvroSerializer()
      val schema: Schema = AvroSchema[T]
      val toRecord: ToRecord[T] = ToRecord.apply[T](schema)

      override def configure(
          configs: java.util.Map[String, ?],
          isKey: Boolean,
      ): Unit = inner.configure(configs, isKey)

      override def serialize(topic: String, maybeData: T): Array[Byte] =
        Option(maybeData)
          .map(data => inner.serialize(topic, toRecord.to(data)))
          .getOrElse(Array.emptyByteArray)

      override def close(): Unit = inner.close()
    }

  def reflectionDeserializer4S[T >: Null: SchemaFor: Decoder]: Deserializer[T] =
    new Deserializer[T] {
      val inner = new GenericAvroDeserializer()
      val schema: Schema = AvroSchema[T]
      val fromRecord: FromRecord[T] = FromRecord.apply[T](schema)

      override def configure(
          configs: java.util.Map[String, ?],
          isKey: Boolean,
      ): Unit = inner.configure(configs, isKey)

      override def deserialize(topic: String, maybeData: Array[Byte]): T =
        Option(maybeData)
          .filter(_.nonEmpty)
          .map(data => fromRecord.from(inner.deserialize(topic, data)))
          .getOrElse(null.asInstanceOf[T])

      override def close(): Unit = inner.close()
    }
}
