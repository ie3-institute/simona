/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test

import edu.ie3.simona.test.KafkaFlatSpec.Topic
import org.apache.kafka.clients.admin.{Admin, NewTopic}
import org.junit.Rule
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, GivenWhenThen, Inspectors}
import org.testcontainers.containers.KafkaContainer
import org.testcontainers.utility.DockerImageName

import scala.jdk.CollectionConverters._

/** Adapted from
  * https://kafka-tutorials.confluent.io/produce-consume-lang/scala.html
  */
trait KafkaFlatSpec
    extends AnyFlatSpec
    with Matchers
    with Inspectors
    with BeforeAndAfterAll
    with GivenWhenThen
    with Eventually {

  val testTopics: Vector[Topic]

  @Rule
  val kafka = new KafkaContainer(
    DockerImageName.parse("confluentinc/cp-kafka:6.1.0")
  )
  lazy val admin: Admin = Admin.create(
    Map[String, AnyRef]("bootstrap.servers" -> kafka.getBootstrapServers).asJava
  )

  override def beforeAll(): Unit = {
    super.beforeAll()
    kafka.start()
    admin.createTopics(
      testTopics.map { topic =>
        new NewTopic(
          topic.name,
          topic.partitions,
          topic.replicationFactor
        )
      }.asJava
    )
  }

  override def afterAll(): Unit = {
    admin.close()
    kafka.stop()
    super.afterAll()
  }
}

object KafkaFlatSpec {
  final case class Topic(
      name: String,
      partitions: Int,
      replicationFactor: Short
  )
}
