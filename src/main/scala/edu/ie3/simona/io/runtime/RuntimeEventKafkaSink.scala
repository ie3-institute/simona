/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.runtime

import edu.ie3.simona.config.ConfigParams.RuntimeKafkaParams
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.{Done, Error}
import edu.ie3.simona.io.runtime.RuntimeEventKafkaSink.SimonaEndMessage
import edu.ie3.simona.io.runtime.RuntimeEventSink.RuntimeStats
import edu.ie3.util.scala.io.ScalaReflectionSerde.reflectionSerializer4S
import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG
import org.apache.kafka.clients.producer.{
  KafkaProducer,
  ProducerConfig,
  ProducerRecord,
}
import org.apache.kafka.common.serialization.{Serdes, Serializer}
import org.slf4j.Logger

import java.util.{Properties, UUID}
import scala.jdk.CollectionConverters._

/** Runtime event sink that sends events related to the simulation ending to a
  * kafka topic.
  *
  * @param producer
  *   the kafka producer to use
  * @param simRunId
  *   the id of this simulation run
  * @param topic
  *   the topic to send the events to
  * @param log
  *   The logger to use
  */
final case class RuntimeEventKafkaSink(
    producer: KafkaProducer[String, SimonaEndMessage],
    simRunId: UUID,
    topic: String,
    log: Logger,
) extends RuntimeEventSink {

  override def handleRuntimeEvent(
      runtimeEvent: RuntimeEvent,
      runtimeStats: RuntimeStats,
  ): Unit = {
    (runtimeEvent match {
      case Done(_, _, errorInSim) =>
        Some(
          SimonaEndMessage(simRunId, runtimeStats.failedPowerFlows, errorInSim)
        )

      case Error(_) =>
        Some(SimonaEndMessage(simRunId, -1, error = true))

      case _ =>
        log.debug(
          s"Logging for event $runtimeEvent not implemented in RuntimeEventKafkaSink."
        )
        None
    }).foreach { msg =>
      producer.send(
        new ProducerRecord[String, SimonaEndMessage](topic, msg)
      )
    }

  }

  override def close(): Unit = {
    producer.flush()
    producer.close()
  }
}

object RuntimeEventKafkaSink {
  def apply(
      config: RuntimeKafkaParams,
      log: Logger,
  ): RuntimeEventKafkaSink = {
    val simRunId = UUID.fromString(config.runId)

    val props = new Properties()
    props.put(ProducerConfig.LINGER_MS_CONFIG, config.linger)
    props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, config.bootstrapServers)
    props.put(
      ProducerConfig.ENABLE_IDEMPOTENCE_CONFIG,
      true,
    ) // exactly once delivery

    val keySerializer = Serdes.String().serializer()
    val valueSerializer: Serializer[SimonaEndMessage] =
      reflectionSerializer4S[SimonaEndMessage]

    valueSerializer.configure(
      Map(SCHEMA_REGISTRY_URL_CONFIG -> config.schemaRegistryUrl).asJava,
      false,
    )

    RuntimeEventKafkaSink(
      new KafkaProducer[String, SimonaEndMessage](
        props,
        keySerializer,
        valueSerializer,
      ),
      simRunId,
      config.topic,
      log,
    )
  }

  final case class SimonaEndMessage(
      simRunId: UUID,
      failedPowerFlows: Int,
      error: Boolean,
  )
}
