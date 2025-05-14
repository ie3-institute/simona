/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.io.result.plain.PlainResult.PlainNodeResult
import edu.ie3.simona.io.result.plain.PlainWriter.NodeResultWriter
import edu.ie3.simona.io.result.plain.{PlainResult, PlainWriter}
import edu.ie3.util.scala.io.ScalaReflectionSerde.reflectionSerializer4S
import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG
import org.apache.kafka.clients.producer.{
  KafkaProducer,
  ProducerConfig,
  ProducerRecord,
}
import org.apache.kafka.common.serialization.{Serdes, Serializer}

import java.util.{Properties, UUID}
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

final case class ResultEntityKafkaSink[
    V <: ResultEntity,
    P <: PlainResult,
] private (
    producer: KafkaProducer[String, P],
    plainWriter: PlainWriter[V, P],
    topic: String,
) extends ResultEntitySink {

  override def handleResultEntity(resultEntity: ResultEntity): Unit = {
    val plainEntity = plainWriter.writePlain(resultEntity.asInstanceOf[V])
    producer.send(
      new ProducerRecord[String, P](topic, plainEntity)
    )
  }

  override def close(): Unit = {
    producer.flush()
    producer.close()
  }
}

object ResultEntityKafkaSink {

  def apply[R](
      topic: String,
      simRunId: UUID,
      bootstrapServers: String,
      schemaRegistryUrl: String,
      linger: Int,
  )(implicit
      tag: ClassTag[R]
  ): ResultEntityKafkaSink[? <: ResultEntity, ? <: PlainResult] = {
    val props = new Properties()
    props.put(ProducerConfig.LINGER_MS_CONFIG, linger)
    props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers)
    props.put(
      ProducerConfig.ENABLE_IDEMPOTENCE_CONFIG,
      true,
    ) // exactly once delivery

    val NodeResClass = classOf[NodeResult]

    tag.runtimeClass match {
      case NodeResClass =>
        implicit val valueSerializer: Serializer[PlainNodeResult] =
          reflectionSerializer4S[PlainNodeResult]
        createSink(schemaRegistryUrl, props, topic, NodeResultWriter(simRunId))
    }
  }

  private def createSink[F <: ResultEntity, P <: PlainResult](
      schemaRegistryUrl: String,
      props: Properties,
      topic: String,
      writer: PlainWriter[F, P],
  )(implicit valueSerializer: Serializer[P]): ResultEntityKafkaSink[F, P] = {
    val keySerializer = Serdes.String().serializer()

    valueSerializer.configure(
      Map(SCHEMA_REGISTRY_URL_CONFIG -> schemaRegistryUrl).asJava,
      false,
    )

    ResultEntityKafkaSink(
      new KafkaProducer[String, P](
        props,
        keySerializer,
        valueSerializer,
      ),
      writer,
      topic,
    )
  }
}
