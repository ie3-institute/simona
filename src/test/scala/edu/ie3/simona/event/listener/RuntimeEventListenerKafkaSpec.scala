/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import com.sksamuel.avro4s.RecordFormat
import edu.ie3.simona.config.{RuntimeConfig, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent.{Done, Error, PowerFlowFailed}
import edu.ie3.simona.io.runtime.RuntimeEventKafkaSink.SimonaEndMessage
import edu.ie3.simona.test.KafkaSpecLike
import edu.ie3.simona.test.KafkaSpecLike.Topic
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.io.ScalaReflectionSerde
import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.{Deserializer, Serdes}
import org.apache.kafka.common.utils.Bytes
import org.scalatest.GivenWhenThen
import org.scalatest.prop.TableDrivenPropertyChecks

import java.util.UUID
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.DurationConverters._
import scala.language.postfixOps

class RuntimeEventListenerKafkaSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with KafkaSpecLike
    with GivenWhenThen
    with TableDrivenPropertyChecks
    with RuntimeTestData {
  private var testConsumer: KafkaConsumer[Bytes, SimonaEndMessage] = _

  private implicit lazy val resultFormat: RecordFormat[SimonaEndMessage] =
    RecordFormat[SimonaEndMessage]
  private val deserializer: Deserializer[SimonaEndMessage] =
    ScalaReflectionSerde.reflectionDeserializer4S[SimonaEndMessage]

  private val testTopic = Topic("testtopic", 1, 1)
  override protected val testTopics: Seq[Topic] = Seq(
    testTopic
  )
  private val topicPartitions: Seq[TopicPartition] =
    (0 until testTopic.partitions).map(
      new TopicPartition(testTopic.name, _)
    )

  private val mockSchemaRegistryUrl = "mock://unused:8081"

  deserializer.configure(
    Map(SCHEMA_REGISTRY_URL_CONFIG -> mockSchemaRegistryUrl).asJava,
    false,
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    val config = Map[String, AnyRef](
      "group.id" -> "test",
      "bootstrap.servers" -> kafka.bootstrapServers,
    )
    testConsumer = new KafkaConsumer[Bytes, SimonaEndMessage](
      config.asJava,
      Serdes.Bytes().deserializer(),
      deserializer,
    )

    testConsumer.assign(topicPartitions.asJava)
  }

  "RuntimeEventListener with Kafka configuration" should {
    "write a series of new RuntimeEvents to Kafka" in {

      Given("a RuntimeEventListener with Kafka config")

      val runId = UUID.randomUUID()

      // build the listener
      val listenerRef = spawn(
        RuntimeEventListener(
          RuntimeConfig.Listener(
            None,
            Some(
              SimonaConfig.RuntimeKafkaParams(
                bootstrapServers = kafka.bootstrapServers,
                linger = 0,
                runId = runId.toString,
                schemaRegistryUrl = mockSchemaRegistryUrl,
                topic = testTopic.name,
              )
            ),
          ),
          None,
          startDateTimeString,
        )
      )

      When("receiving the SimonaEndMessages")

      val cases = Table(
        ("event", "expectedMsg"),
        (
          Done(1800L, duration, errorInSim = false),
          SimonaEndMessage(runId, 1, error = false),
        ),
        (
          Done(3600L, duration, errorInSim = true),
          SimonaEndMessage(runId, 1, error = true),
        ),
        (Error(errMsg), SimonaEndMessage(runId, -1, error = true)),
      )

      Then("records can be fetched from Kafka")

      testConsumer.seekToBeginning(topicPartitions.asJava)

      // one failed power flow
      listenerRef ! PowerFlowFailed

      forAll(cases) { case (event, expectedMsg) =>
        listenerRef ! event

        val receivedRecord =
          eventually(timeout(20 seconds), interval(1 second)) {
            val records =
              testConsumer.poll((1 second) toJava).asScala.map(_.value()).toList

            // run until one record is received. After each second, if no record
            // was received, the length check below fails and we retry
            records should have length 1

            // return final record to be checked
            records.headOption.value
          }

        receivedRecord shouldBe expectedMsg

      }

    }
  }
}
