/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import com.sksamuel.avro4s.RecordFormat
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent.{Done, Error}
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
    with KafkaSpecLike
    with UnitSpec
    with GivenWhenThen
    with TableDrivenPropertyChecks {
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

  private val startDateTimeString = "2011-01-01 00:00:00"

  private val mockSchemaRegistryUrl = "mock://unused:8081"

  deserializer.configure(
    Map(SCHEMA_REGISTRY_URL_CONFIG -> mockSchemaRegistryUrl).asJava,
    false
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    val config = Map[String, AnyRef](
      "group.id" -> "test",
      "bootstrap.servers" -> kafka.bootstrapServers
    )
    testConsumer = new KafkaConsumer[Bytes, SimonaEndMessage](
      config.asJava,
      Serdes.Bytes().deserializer(),
      deserializer
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
          SimonaConfig.Simona.Runtime.Listener(
            None,
            Some(
              SimonaConfig.RuntimeKafkaParams(
                bootstrapServers = kafka.bootstrapServers,
                linger = 0,
                runId = runId.toString,
                schemaRegistryUrl = mockSchemaRegistryUrl,
                topic = testTopic.name
              )
            )
          ),
          None,
          startDateTimeString
        )
      )

      When("receiving the SimonaEndMessages")

      val errMsg = "Test error msg"
      val cases = Table(
        ("event", "expectedMsg"),
        (
          Done(1800L, 3L, 0, errorInSim = false),
          SimonaEndMessage(runId, 0, error = false)
        ),
        (
          Done(3600L, 3L, 2, errorInSim = true),
          SimonaEndMessage(runId, 2, error = true)
        ),
        (Error(errMsg), SimonaEndMessage(runId, -1, error = true))
      )

      Then("records can be fetched from Kafka")

      testConsumer.seekToBeginning(topicPartitions.asJava)

      forAll(cases) { case (event, expectedMsg) =>
        listenerRef ! event

        eventually(timeout(20 seconds), interval(1 second)) {
          val records: List[SimonaEndMessage] =
            testConsumer.poll((1 second) toJava).asScala.map(_.value()).toList

          records should have length 1
          records should contain(expectedMsg)
        }
      }

    }
  }
}
