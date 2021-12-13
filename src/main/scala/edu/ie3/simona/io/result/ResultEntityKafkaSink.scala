/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import com.sksamuel.avro4s.RecordFormat
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.io.result.ResultEntityKafkaSink.PlainWriter
import edu.ie3.simona.io.result.plain.{NodeResultPlain, ResultPlain}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.io.ScalaReflectionSerde.reflectionSerializer4S
import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG
import org.apache.kafka.clients.producer.{
  KafkaProducer,
  ProducerConfig,
  ProducerRecord
}
import org.apache.kafka.common.serialization.{Serdes, Serializer}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.{Properties, UUID}
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

final case class ResultEntityKafkaSink[
    V <: ResultEntity,
    P <: ResultPlain
] private (
    producer: KafkaProducer[String, P],
    plainWriter: PlainWriter[V, P],
    topic: String
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
      runId: UUID,
      bootstrapServers: String,
      schemaRegistryUrl: String,
      linger: Int
  )(implicit
      tag: ClassTag[R]
  ): ResultEntityKafkaSink[_ <: ResultEntity, _ <: ResultPlain] = {
    val props = new Properties()
    props.put(ProducerConfig.LINGER_MS_CONFIG, linger)
    props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers)
    props.put(
      ProducerConfig.ENABLE_IDEMPOTENCE_CONFIG,
      true
    ) // exactly once delivery

    val NodeResClass = classOf[NodeResult]

    tag.runtimeClass match {
      case NodeResClass =>
        implicit val recordFormat: RecordFormat[NodeResultPlain] =
          RecordFormat[NodeResultPlain]
        createSink(schemaRegistryUrl, props, topic, NodeResultWriter(runId))
    }
  }

  private def createSink[F <: ResultEntity, P <: ResultPlain: RecordFormat](
      schemaRegistryUrl: String,
      props: Properties,
      topic: String,
      writer: PlainWriter[F, P]
  ): ResultEntityKafkaSink[F, P] = {
    val keySerializer = Serdes.String().serializer()
    val valueSerializer: Serializer[P] = reflectionSerializer4S[P]

    valueSerializer.configure(
      Map(SCHEMA_REGISTRY_URL_CONFIG -> schemaRegistryUrl).asJava,
      false
    )

    ResultEntityKafkaSink(
      new KafkaProducer[String, P](
        props,
        keySerializer,
        valueSerializer
      ),
      writer,
      topic
    )
  }

  trait PlainWriter[F <: ResultEntity, P <: ResultPlain] {
    def writePlain(full: F): P

    def createFull(plain: P): F
  }

  case class NodeResultWriter(runId: UUID)
      extends PlainWriter[NodeResult, NodeResultPlain] {
    override def writePlain(full: NodeResult): NodeResultPlain =
      NodeResultPlain(
        runId,
        full.getTime.toString,
        full.getUuid,
        full.getvMag().getValue.doubleValue(),
        full.getvAng().getValue.doubleValue()
      )

    override def createFull(plain: NodeResultPlain): NodeResult = {
      new NodeResult(
        ZonedDateTime.parse(plain.dateTime),
        plain.uuid,
        Quantities.getQuantity(plain.vMagPU, PowerSystemUnits.PU),
        Quantities.getQuantity(plain.vAngDeg, PowerSystemUnits.DEGREE_GEOM)
      )
    }
  }
}
