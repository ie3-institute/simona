/*
 * © 2025. TU Dortmund University,
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

trait KafkaSpecLike extends BeforeAndAfterAll { this: TestSuite =>

  protected val testTopics: Seq[Topic]

  protected lazy val kafka: KafkaContainer =
    KafkaContainer(DockerImageName.parse("apache/kafka:3.7.0"))

  protected lazy val admin: Try[Admin] =
    if kafka.container.isRunning then
      Success(
        Admin.create(Map("bootstrap.servers" -> kafka.bootstrapServers).asJava)
      )
    else
      Failure(
        new IllegalStateException(
          "Kafka container must be started before creating Admin client"
        )
      )

  override def beforeAll(): Unit = {
    super.beforeAll()
    kafka.start()

    val result = admin.map(
      _.createTopics(
        testTopics
          .map(t => new NewTopic(t.name, t.partitions, t.replicationFactor))
          .asJava
      )
    )
    result.map(_.all().get(15, TimeUnit.SECONDS))
  }

  override def afterAll(): Unit = {
    if admin != null then admin.map(_.close())
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
