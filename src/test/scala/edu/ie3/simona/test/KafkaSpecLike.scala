/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test

import com.dimafeng.testcontainers.KafkaContainer
import edu.ie3.simona.test.KafkaSpecLike.Topic
import org.apache.kafka.clients.admin.{Admin, NewTopic}
import org.scalatest.{BeforeAndAfterAll, TestSuite}
import org.testcontainers.utility.DockerImageName

import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** Adapted from
  * https://kafka-tutorials.confluent.io/produce-consume-lang/scala.html
  */
trait KafkaSpecLike extends BeforeAndAfterAll { this: TestSuite =>

  /** Topics that should exist in the test broker */
  protected val testTopics: Seq[Topic]

  /** Kafka container definition; started in [[beforeAll]] */
  protected lazy val kafka: KafkaContainer =
    KafkaContainer(DockerImageName.parse("apache/kafka:3.7.0"))

  /** Create an Admin client once the container is running */
  protected def createAdmin(): Admin =
    Admin.create(Map("bootstrap.servers" -> kafka.bootstrapServers).asJava)

  override def beforeAll(): Unit = {
    super.beforeAll()
    kafka.start()

    val result = Try {
      val admin = createAdmin()
      try {
        val topics = testTopics
          .map(t => new NewTopic(t.name, t.partitions, t.replicationFactor))
          .asJava
        admin.createTopics(topics).all().get(15, TimeUnit.SECONDS)
      } finally {
        admin.close()
      }
    }

    result match {
      case Success(_) =>
      case Failure(ex) =>
        throw new IllegalStateException("Failed to create Kafka topics", ex)
    }
  }

  override def afterAll(): Unit = {
    kafka.stop()
    super.afterAll()
  }
}

object KafkaSpecLike {
  final case class Topic(
      name: String,
      partitions: Int,
      replicationFactor: Short,
  )
}
