/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.{EventFilter, TestActorRef}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.event.SimonaListenerSpec.{TestEvent, UnknownEvent}
import edu.ie3.simona.event.listener.SimonaListenerWithFilter
import edu.ie3.simona.logging.SimonaLogging.SimonaBusLogging
import edu.ie3.simona.test.common.TestKitWithShutdown
import org.scalatest.matchers.should.Matchers

import java.util.{Calendar, Date}

object SimonaListenerSpec {

  // test classes
  final case class TestEvent(str: String, date: Date) extends Event

  final case object UnknownEvent extends Event

}

class SimonaListenerSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "SimonaListenerSpec",
        ConfigFactory
          .parseString(
            """
            |pekko.loggers =["edu.ie3.simona.test.common.SilentTestEventListener"]
            |pekko.loglevel="debug"
            |""".stripMargin
          ),
      )
    )
    with Matchers {

  // test listenerActor
  class SimonaListenerActor(eventsToProcess: Option[List[String]] = None)
      extends SimonaListenerWithFilter(eventsToProcess) {
    override def preStart(): Unit = {
      log.debug(s"{} started!", self)
    }

    override def processEvent(event: Event, sender: ActorRef): Unit = {
      event match {
        case TestEvent(str, date) =>
          log.debug(s"Received '$str' from date $date")
        case _ => log.warning("Received unknown event")
      }
    }
  }

  // global vals
  private val listener = TestActorRef(
    new SimonaListenerActor
  )

  private val logPrefix = listener.underlyingActor.log match {
    case simonaLogging: SimonaBusLogging =>
      simonaLogging.prefix()
    case x =>
      throw new IllegalArgumentException(
        s"Invalid logger in RuntimeEventListener: $x"
      )
  }

  "A simple SimonaListener" should {
    "be able receive an event and process it if it is expected" in {
      val msgDate = Calendar.getInstance().getTime
      val msg = "Hello World"
      EventFilter
        .debug(
          message = s"$logPrefix Received '$msg' from date $msgDate",
          occurrences = 1,
        )
        .intercept {
          listener ! TestEvent(msg, msgDate)
        }
    }
  }

  "A simple listener" should {
    "process an unknown Event differently than any other unknown object" in {
      EventFilter
        .warning(
          message = s"$logPrefix Received unknown event",
          occurrences = 1,
        )
        .intercept {
          listener ! UnknownEvent
        }
      val unknownMessage = "StringMessage"
      EventFilter
        .warning(
          message = s"$logPrefix Received unknown message: $unknownMessage",
          occurrences = 1,
        )
        .intercept {
          listener ! unknownMessage
        }
    }
  }

}
