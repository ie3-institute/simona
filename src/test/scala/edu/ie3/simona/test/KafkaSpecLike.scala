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

trait KafkaSpecLike extends BeforeAndAfterAll { this: TestSuite =>

  protected val testTopics: Seq[Topic]

  private var kafka: KafkaContainer = _
  private var admin: Admin = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    // create and start container here – not during trait init
    kafka = KafkaContainer(DockerImageName.parse("apache/kafka:3.7.0"))
    kafka.start()

    // now ports exist; safe to build Admin client
    admin = Admin.create(
      Map[String, AnyRef]("bootstrap.servers" -> kafka.bootstrapServers).asJava
    )

    val result = admin.createTopics(
      testTopics
        .map(t => new NewTopic(t.name, t.partitions, t.replicationFactor))
        .asJava
    )
    result.all().get(1, TimeUnit.MINUTES)
  }

  override def afterAll(): Unit = {
    if admin != null then admin.close()
    if kafka != null then kafka.stop()
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
