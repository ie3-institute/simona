/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test

import com.dimafeng.testcontainers.KafkaContainer
import edu.ie3.simona.test.KafkaSpecLike.Topic
import org.apache.kafka.clients.admin.{Admin, NewTopic}
import org.junit.Rule
import org.scalatest.{BeforeAndAfterAll, TestSuite}
import org.testcontainers.utility.DockerImageName

import scala.jdk.CollectionConverters._

/** Adapted from
  * https://kafka-tutorials.confluent.io/produce-consume-lang/scala.html
  */
trait KafkaSpecLike extends BeforeAndAfterAll {
  this: TestSuite =>

  protected val testTopics: Seq[Topic]

  @Rule
  protected val kafka: KafkaContainer = KafkaContainer(
    DockerImageName.parse("confluentinc/cp-kafka:6.1.0")
  )
  protected lazy val admin: Admin = Admin.create(
    Map[String, AnyRef]("bootstrap.servers" -> kafka.bootstrapServers).asJava
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

object KafkaSpecLike {
  final case class Topic(
      name: String,
      partitions: Int,
      replicationFactor: Short
  )
}
