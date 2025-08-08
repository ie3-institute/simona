/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import java.util.{Calendar, Date}
import org.apache.pekko.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  ActorSystem,
  Props,
}
import org.apache.pekko.testkit.ImplicitSender
import org.apache.pekko.util.Timeout
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.result.system._
import edu.ie3.simona.event.NotifierSpec.{TestEvent, TestEventEnvelope}
import edu.ie3.simona.event.notifier.Notifier
import edu.ie3.simona.test.common.TestKitWithShutdown
import edu.ie3.simona.util.ConfigUtil.NotifierIdentifier._
import edu.ie3.simona.util.EntityMapperUtil
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.language.postfixOps

class NotifierSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "NotifierSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers =["org.apache.pekko.testkit.TestEventListener"]
            |pekko.loglevel="OFF"
            |""".stripMargin),
      )
    )
    with Matchers
    with ImplicitSender {

  // test listenerActor
  class NotifierActor(override val listener: Iterable[ActorRef])
      extends Notifier
      with Actor
      with ActorLogging {
    override def preStart(): Unit = {
      log.debug(s"{} started!", self)
    }

    override def receive: Receive = {
      case TestEventEnvelope(testEvent, "Please notify others of this!") =>
        log.info(s"Received event $testEvent, will now notify my listeners")
        notifyListener(testEvent)
      case unknown => log.warning(s"Received this unknown message: $unknown")
    }
  }

  // global vals
  // Publisher Actor has 'self' as listener, which is possible through the mix in of 'ImplicitSender'
  val notifier: ActorRef =
    system.actorOf(Props(new NotifierActor(Iterable(self))))

  "A simple Notifier" should {
    "be able to notify his listeners of an event" in {
      val msgDate = Calendar.getInstance().getTime
      val msg = "Hello World"
      val testEvent = TestEvent(msg, msgDate)
      implicit val timeout: Timeout = Timeout(5 seconds)
      notifier ! TestEventEnvelope(testEvent)
      expectMsg(testEvent)
    }
  }

  "The notifier object" should {
    "provide notifier to result entity mappings" in {
      val entityMapping = Map(
        PvPlant ->
          classOf[PvResult],
        Wec ->
          classOf[WecResult],
        Load ->
          classOf[LoadResult],
        FixedFeedIn ->
          classOf[FixedFeedInResult],
        BioMassPlant ->
          classOf[BmResult],
        Evcs ->
          classOf[EvcsResult],
        Storage ->
          classOf[StorageResult],
        Ev ->
          classOf[EvResult],
        Em ->
          classOf[EmResult],
      )
      // TODO: Grid results are not covered, yet.

      entityMapping.forall { case (emitter, entityClass) =>
        EntityMapperUtil.getResultEntityClass(emitter).equals(entityClass)
      } shouldBe true
    }
  }
}

object NotifierSpec {

  // test classes
  final case class TestEvent(str: String, date: Date) extends Event

  final case class TestEventEnvelope(
      testEvent: TestEvent,
      msg: String = "Please notify others of this!",
  )

}
