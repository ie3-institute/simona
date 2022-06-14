/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import akka.actor.ActorSystem
import akka.testkit.TestActorRef
import com.sksamuel.avro4s.RecordFormat
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.event.listener.ResultEventListener
import edu.ie3.simona.io.result.plain.PlainResult.PlainNodeResult
import edu.ie3.simona.io.result.plain.PlainWriter
import edu.ie3.simona.test.KafkaSpecLike
import edu.ie3.simona.test.KafkaSpecLike.Topic
import edu.ie3.simona.test.common.TestKitWithShutdown
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.io.ScalaReflectionSerde
import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.{Deserializer, Serdes}
import org.apache.kafka.common.utils.Bytes
import org.scalatest.GivenWhenThen
import org.scalatest.concurrent.Eventually
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.DurationConverters._
import scala.language.postfixOps

/** Adapted from
  * https://kafka-tutorials.confluent.io/produce-consume-lang/scala.html
  */
class ResultEntityKafkaSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ResultEntityKafkaSpec",
        ConfigFactory
          .parseString(
            """akka.loggers = ["edu.ie3.simona.test.common.SilentTestEventListener"]
          |akka.loglevel="info"
          """.stripMargin
          )
      )
    )
    with KafkaSpecLike
    with GivenWhenThen
    with Eventually {

  var testConsumer: KafkaConsumer[Bytes, PlainNodeResult] = _

  private implicit lazy val resultFormat: RecordFormat[PlainNodeResult] =
    RecordFormat[PlainNodeResult]
  val deserializer: Deserializer[PlainNodeResult] =
    ScalaReflectionSerde.reflectionDeserializer4S[PlainNodeResult]

  private val topic = "testtopic"

  override val testTopics: Vector[Topic] = Vector(
    Topic(topic, 1, 1)
  )

  deserializer.configure(
    Map(SCHEMA_REGISTRY_URL_CONFIG -> "mock://unused:8081").asJava,
    false
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    val config = Map[String, AnyRef](
      "group.id" -> "test",
      "bootstrap.servers" -> kafka.bootstrapServers
    )
    testConsumer = new KafkaConsumer[Bytes, PlainNodeResult](
      config.asJava,
      Serdes.Bytes().deserializer(),
      deserializer
    )
  }

  "ResultEventListener with Kafka configuration" should {
    "write a series of new NodeResults to Kafka" in {

      Given("a ResultEventListener with Kafka config")

      val mockSchemaRegistryUrl = "mock://unused:8081"

      val runId = UUID.randomUUID()

      // build the listener
      val listenerRef = TestActorRef(
        ResultEventListener.props(
          ResultFileHierarchy(
            "out",
            "simName",
            ResultFileHierarchy.ResultEntityPathConfig(
              Set(classOf[NodeResult]),
              ResultSinkType.Kafka(
                topic,
                runId,
                kafka.bootstrapServers,
                mockSchemaRegistryUrl,
                0
              )
            )
          ),
          testActor
        )
      )

      And("a collection of NodeResults")
      val nodeRes1 = new NodeResult(
        ZonedDateTime.parse("2021-01-01T00:00:00+01:00[Europe/Berlin]"),
        UUID.randomUUID(),
        Quantities.getQuantity(1d, PowerSystemUnits.PU),
        Quantities.getQuantity(0d, PowerSystemUnits.DEGREE_GEOM)
      )
      val nodeRes2 = new NodeResult(
        ZonedDateTime.parse("2021-01-01T00:00:00+01:00[Europe/Berlin]"),
        UUID.randomUUID(),
        Quantities.getQuantity(0.8d, PowerSystemUnits.PU),
        Quantities.getQuantity(15d, PowerSystemUnits.DEGREE_GEOM)
      )
      val nodeRes3 = new NodeResult(
        ZonedDateTime.parse("2021-01-10T00:00:00+01:00[Europe/Berlin]"),
        UUID.randomUUID(),
        Quantities.getQuantity(0.75d, PowerSystemUnits.PU),
        Quantities.getQuantity(90d, PowerSystemUnits.DEGREE_GEOM)
      )

      When("receiving the NodeResults")
      listenerRef ! PowerFlowResultEvent(
        Iterable(nodeRes1, nodeRes2, nodeRes3),
        Iterable.empty,
        Iterable.empty,
        Iterable.empty,
        Iterable.empty
      )

      val testTopic = testTopics.headOption.value
      val topicPartitions: Seq[TopicPartition] =
        (0 until testTopic.partitions).map(
          new TopicPartition(testTopic.name, _)
        )

      testConsumer.assign(topicPartitions.asJava)

      Then("records can be fetched from Kafka")
      eventually(timeout(5 second), interval(1 second)) {
        testConsumer.seekToBeginning(topicPartitions.asJava)
        val records: List[PlainNodeResult] =
          testConsumer.poll((1 second) toJava).asScala.map(_.value()).toList

        records should have length 3
        records should contain(
          PlainNodeResult(
            runId,
            PlainWriter.createSimpleTimeStamp(nodeRes1.getTime),
            nodeRes1.getUuid,
            nodeRes1.getInputModel,
            nodeRes1.getvMag().getValue.doubleValue(),
            nodeRes1.getvAng().getValue.doubleValue()
          )
        )
        records should contain(
          PlainNodeResult(
            runId,
            PlainWriter.createSimpleTimeStamp(nodeRes2.getTime),
            nodeRes2.getUuid,
            nodeRes2.getInputModel,
            nodeRes2.getvMag().getValue.doubleValue(),
            nodeRes2.getvAng().getValue.doubleValue()
          )
        )
        records should contain(
          PlainNodeResult(
            runId,
            PlainWriter.createSimpleTimeStamp(nodeRes3.getTime),
            nodeRes3.getUuid,
            nodeRes3.getInputModel,
            nodeRes3.getvMag().getValue.doubleValue(),
            nodeRes3.getvAng().getValue.doubleValue()
          )
        )
      }
    }
  }
}
