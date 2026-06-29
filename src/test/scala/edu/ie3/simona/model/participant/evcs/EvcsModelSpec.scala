/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsOperatingPoint,
  EvcsState,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.Data.SecondaryData.ArrivingEvs
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.helper.TableDrivenHelper
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.*
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.Kilovars
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Each, Energy, Power}

import java.time.ZonedDateTime

class EvcsModelSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with TableDrivenHelper
    with EvcsInputTestData {

  private val dateTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T03:04:05Z")

  // Testing tolerances
  given Energy = KilowattHours(1e-10)
  given Power = Kilowatts(1e-10)

  "An EVCS model" should {

    "calculate new schedules correctly" when {

      "configured with max power charging" in {
        val evcsModel = createTestModel("maxPower")

        val evModel = EvModelWrapper(
          ev3.copyWith(5.0.asKiloWattHour)
        )

        val (operatingPoint, nextEvent) = evcsModel.determineOperatingPoint(
          EvcsState(
            Seq(evModel),
            3600L,
          )
        )

        operatingPoint.evOperatingPoints shouldBe Map(
          evModel.uuid ->
            // ending early at 9000 because of max power charging
            Kilowatts(10.0)
        )

        nextEvent shouldBe Some(9000L)
      }

      "configured with constant power charging" in {
        val evcsModel = createTestModel("constantPower")

        val evModel = EvModelWrapper(ev3)

        val (operatingPoint, nextEvent) = evcsModel.determineOperatingPoint(
          EvcsState(
            Seq(evModel),
            3600L,
          )
        )

        operatingPoint.evOperatingPoints shouldBe Map(
          evModel.uuid ->
            // using 2.5 kW with constant power charging
            Kilowatts(2.5)
        )
        nextEvent shouldBe Some(10800L)
      }

      "Ev is fully charged" in {
        val evcsModel = createTestModel("maxPower")

        val evModel = EvModelWrapper(
          ev1.copyWith(10.0.asKiloWattHour)
        )

        val (operatingPoint, nextEvent) = evcsModel.determineOperatingPoint(
          EvcsState(
            Seq(evModel),
            1800L,
          )
        )
        operatingPoint.evOperatingPoints shouldBe Map(
          evModel.uuid -> Kilowatts(0.0)
        )
        nextEvent shouldBe None
      }
    }

    "handling power tolerance during operating point determination" in {
      val evcsModel = createTestModel("constantPower")
      val ev = EvModelWrapper(
        ev1.copyWith(5.0.asKiloWattHour)
      )
      val tolerance = evcsModel.calcPowerTolerance(ev)
      val tinyPower = tolerance / 10
      val validPower = tolerance * 10

      tinyPower.~=(zeroKW)(using tolerance) shouldBe true

      val state = EvcsState(Seq(ev), 0L)
      val op = evcsModel.determineOperatingPoint(state, tinyPower)
      op.evOperatingPoints(ev.uuid) shouldBe zeroKW

      evcsModel.determineChargingLimitEvent(ev, tinyPower, 0L) shouldBe None

      evcsModel.determineChargingLimitEvent(
        ev,
        validPower,
        0L,
      ) should not be empty
    }
  }

  "determining current state correctly" when {

    "being provided with a ChargingSchedule consisting of one entry" in {
      val evcsModel = createTestModel("constantPower")

      val cases = Table(
        (
          "storedEnergy",
          "lastStateTick",
          "currentTick",
          "power",
          "expectedStored",
        ),
        // empty battery
        (0.0, 900L, 2700L, 5.0, 2.5),
        (0.0, 0L, 3600L, -5.0, 0.0),
        // half full battery
        (5.0, 0L, 3600L, 5.0, 10.0),
        (5.0, 900L, 2700L, 5.0, 7.5),
        (5.0, 0L, 3600L, -5.0, 0.0),
        (5.0, 900L, 2700L, -5.0, 2.5),
        // full battery
        (10.0, 900L, 2700L, -5.0, 7.5),
        (10.0, 0L, 3600L, 5.0, 10.0),
      )

      forAll(cases) {
        (
            storedEnergy,
            lastStateTick,
            currentTick,
            power,
            expectedStored,
        ) =>
          val ev = EvModelWrapper(
            ev1.copyWith(storedEnergy.asKiloWattHour)
          )

          val state = EvcsState(
            Seq(ev),
            lastStateTick,
          )

          val operatingPoint = EvcsOperatingPoint(
            Map(ev.uuid -> Kilowatts(power))
          )

          val newState = evcsModel.determineState(
            state,
            operatingPoint,
            currentTick,
            dateTime,
          )

          newState.evs should have size 1
          newState.tick shouldBe currentTick

          val actualEv = newState.evs.headOption.getOrElse(
            fail("No charging schedule provided.")
          )

          actualEv.uuid shouldBe ev.uuid
          actualEv.id shouldBe ev.id
          actualEv.cosPhi shouldBe ev.cosPhi
          actualEv.sRatedAc shouldBe ev.sRatedAc
          actualEv.pRatedAc shouldBe ev.pRatedAc
          actualEv.pRatedDc shouldBe ev.pRatedDc
          actualEv.eStorage shouldBe ev.eStorage
          actualEv.storedEnergy should approximate(
            KilowattHours(expectedStored)
          )
          actualEv.departureTick shouldBe ev.departureTick

      }

    }
  }

  "calculate results correctly" when {

    val evcsModel = createTestModel("constantPower")

    val evA = EvModelWrapper(ev1)
    val evB = EvModelWrapper(ev2)

    "two EVs are parked and charging without last operating point" in {

      val currentOperatingPoint = EvcsOperatingPoint(
        Map(evA.uuid -> Kilowatts(3.0), evB.uuid -> Kilowatts(2.0))
      )

      val state = EvcsState(
        Seq(evA, evB),
        10800L,
      )

      val results = evcsModel.createResults(
        state,
        None,
        currentOperatingPoint,
        ComplexPower(Kilowatts(5), Kilovars(0.005)),
        dateTime,
      )

      results should have size 3

      results.foreach {
        case evResult: EvResult if evResult.getInputModel == evA.uuid =>
          evResult.getTime shouldBe dateTime
          evResult.getP should beEquivalentTo(3.0.asKiloWatt)
          evResult.getQ should beEquivalentTo(0.0.asKiloVar)
          evResult.getSoc should beEquivalentTo(50.0.asPercent)
        case evResult: EvResult if evResult.getInputModel == evB.uuid =>
          evResult.getTime shouldBe dateTime
          evResult.getP should beEquivalentTo(2.0.asKiloWatt)
          evResult.getQ should beEquivalentTo(0.0.asKiloVar)
          evResult.getSoc should beEquivalentTo(75.0.asPercent)
        case evcsResult: EvcsResult =>
          evcsResult.getInputModel shouldBe evcsModel.uuid
          evcsResult.getTime shouldBe dateTime
          evcsResult.getP should beEquivalentTo(5.0.asKiloWatt)
          evcsResult.getQ should beEquivalentTo(0.005.asKiloVar)
        case unexpected =>
          fail(s"Unexpected result $unexpected was found.")
      }

    }

    "two EVs are parked and charging with given last operating point" in {

      val lastOperatingPoint = EvcsOperatingPoint(
        Map(evA.uuid -> Kilowatts(3.0), evB.uuid -> Kilowatts(2.0))
      )

      val state = EvcsState(
        Seq(evA, evB),
        10800L,
      )

      val cases = Table(
        ("ev1P", "ev2P", "ev1Res", "ev2Res", "evcsRes"),
        (4.0, 3.0, true, true, true),
        (4.0, 1.0, true, true, false),
        (3.0, 1.0, false, true, true),
        (3.0, 2.0, false, false, false),
      )

      forAll(cases) { (ev1P, ev2P, ev1Res, ev2Res, evcsRes) =>
        val evcsP = ev1P + ev2P
        val evcsQ = evcsP / 100

        val currentOperatingPoint = EvcsOperatingPoint(
          Map(evA.uuid -> Kilowatts(ev1P), evB.uuid -> Kilowatts(ev2P))
        )

        val results = evcsModel.createResults(
          state,
          Some(lastOperatingPoint),
          currentOperatingPoint,
          ComplexPower(Kilowatts(evcsP), Kilovars(evcsQ)),
          dateTime,
        )

        val expectedResults = Iterable(ev1Res, ev2Res, evcsRes).map {
          if _ then 1 else 0
        }.sum

        results should have size expectedResults

        val actualEv1Result = results.find(_.getInputModel == evA.uuid)
        actualEv1Result.isDefined shouldBe ev1Res
        actualEv1Result.foreach {
          case evResult: EvResult =>
            evResult.getTime shouldBe dateTime
            evResult.getP should beEquivalentTo(ev1P.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(50.0.asPercent)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

        val actualEv2Result = results.find(_.getInputModel == evB.uuid)
        actualEv2Result.isDefined shouldBe ev2Res
        actualEv2Result.foreach {
          case evResult: EvResult =>
            evResult.getTime shouldBe dateTime
            evResult.getP should beEquivalentTo(ev2P.asKiloWatt)
            evResult.getQ should beEquivalentTo(0.0.asKiloVar)
            evResult.getSoc should beEquivalentTo(75.0.asPercent)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }

        val actualEvcsResult = results.find(_.getInputModel == evcsModel.uuid)
        actualEvcsResult.isDefined shouldBe evcsRes
        actualEvcsResult.foreach {
          case evcsResult: EvcsResult =>
            evcsResult.getTime shouldBe dateTime
            evcsResult.getP should beEquivalentTo(evcsP.asKiloWatt)
            evcsResult.getQ should beEquivalentTo(evcsQ.asKiloVar)
          case unexpected =>
            fail(s"Unexpected result $unexpected was found.")
        }
      }
    }

  }

  "handle power control correctly" when {

    val evcsModel = createTestModel(
      chargingStrategy = "constantPower",
      departureTargetSoc = 0.8,
    )

    val currentTick = 3600L

    "dealing with one ev" in {

      val cases = Table(
        (
          "stored",
          "setPower",
          "expPower",
        ),

        /* setPower is 0 kW */
        (0.0, 0.0, 0.0),

        /* setPower is positive (charging) */
        (0.0, 4.0, 4.0),
        (4.0, 4.0, 4.0),

        /* setPower is set to > ev (charging) */
        (0.0, 11.0, 10.0),
        (5.0, 15.0, 10.0),

        /* setPower is negative (discharging) */
        (10.0, -6.0, -6.0),
        (5.0, -10.0, -10.0),

        /* setPower is set to > ev (discharging) */
        (10.0, -11.0, -10.0),
        (5.0, -15.0, -10.0),
      )

      forAll(cases) {
        (
            stored: Double,
            setPower: Double,
            expPower: Double,
        ) =>
          // 10 kWh capacity, 10 kWh target, 10 kW max power, stays two hours
          val ev = EvModelWrapper(
            ev4
              .copyWith(stored.asKiloWattHour)
              .copyWithDeparture(currentTick + 7200L)
          )

          evcsModel
            .determineOperatingPoint(
              EvcsState(Seq(ev), currentTick),
              Kilowatts(setPower),
            )
            .evOperatingPoints
            .get(ev.uuid)
            .value shouldBe Kilowatts(expPower)
      }
    }

    "dealing with two evs" in {

      val cases = Table(
        (
          "stored1",
          "stored2",
          "setPower",
          "expPower1",
          "expPower2",
        ),

        /* setPower is 0 kW */
        (0.0, 0.0, 0.0, 0.0, 0.0),
        (10.0, 5.0, 0.0, 0.0, 0.0),
        (5.0, 15.0, 0.0, 0.0, 0.0),

        /* setPower is positive (charging) */
        (0.0, 0.0, 4.0, 0.0, 4.0),
        (0.0, 10.0, 4.0, 2.0, 2.0),
        (10.0, 14.0, 4.0, 0.0, 4.0),

        /* setPower is set to > (ev2 * 2) (charging) */
        (7.0, 0.0, 11.0, 6.0, 5.0),
        (0.0, 5.0, 15.0, 10.0, 5.0),
        (5.0, 7.5, 15.0, 10.0, 5.0),

        /* setPower is negative (discharging) */
        (10.0, 15.0, -4.0, -2.0, -2.0),
        (0.0, 4.0, -4.0, 0.0, -4.0),
        (7.5, 0.0, -5.0, -5.0, 0.0),

        /* setPower is set to > (ev2 * 2) (discharging) */
        (10.0, 15.0, -13.0, -8.0, -5.0),
        (5.0, 15.0, -15.0, -10.0, -5.0),
        (10.0, 15.0, -15.0, -10.0, -5.0),
      )

      forAll(cases) {
        (
            stored1: Double,
            stored2: Double,
            setPower: Double,
            expPower1: Double,
            expPower2: Double,
        ) =>
          // 10 kWh capacity, 10 kWh target, 10 kW max power, stays one hour
          val evA = EvModelWrapper(
            ev4.copyWith(stored1.asKiloWattHour).copyWithDeparture(7200L)
          )
          // 15 kWh capacity, 15 kWh target, 5 kW max power, stays two hours
          val evB = EvModelWrapper(
            ev5.copyWith(stored2.asKiloWattHour).copyWithDeparture(10800L)
          )

          val op = evcsModel.determineOperatingPoint(
            EvcsState(
              Seq(evA, evB),
              currentTick,
            ),
            Kilowatts(setPower),
          )

          op.evOperatingPoints
            .get(evA.uuid)
            .value shouldBe Kilowatts(expPower1)
          op.evOperatingPoints
            .get(evB.uuid)
            .value shouldBe Kilowatts(expPower2)

      }

    }

  }

  "handle arrivals correctly" in {
    val evcsModel = createTestModel("maxPower")

    val state = EvcsState(
      Seq(EvModelWrapper(ev1)),
      3600L,
    )

    val newState = evcsModel.handleInput(
      state = state,
      receivedData = Seq(ArrivingEvs(Seq(EvModelWrapper(ev2)))),
      nodalVoltage = Each(1.0),
    )

    newState shouldBe EvcsState(
      Seq(EvModelWrapper(ev1), EvModelWrapper(ev2)),
      3600L,
    )
  }

  "reply to requests" when {
    val evcsModel = createTestModel("constantPower")

    val evModel = EvModelWrapper(
      ev3.copyWith(5.0.asKiloWattHour)
    )

    // dummy agent because we need a context
    def testAgent(
        model: EvcsModel,
        state: EvcsState,
    ): Behavior[ParticipantAgent.Message] = Behaviors.receivePartial {
      case (ctx, request: DirectAgentRequest) =>
        val newState = model.handleRequest(
          state,
          ctx,
          request,
        )

        testAgent(model, newState)
    }

    "no EVs are parked" in {
      val service = createTestProbe[ServiceResponseMessage]()
      val currentTick = 0L

      val startingState = EvcsState(Seq.empty, currentTick)
      val agent = spawn(testAgent(evcsModel, startingState))

      agent ! EvFreeLotsRequest(currentTick, service.ref)
      service.expectMessage(FreeLotsResponse(evcsModel.uuid, 2))
    }

    "one EV is parked, departing later" in {
      val service = createTestProbe[ServiceResponseMessage]()
      val currentTick = 0L

      val startingState = EvcsState(Seq(evModel), currentTick)
      val agent = spawn(testAgent(evcsModel, startingState))

      agent ! EvFreeLotsRequest(currentTick, service.ref)
      service.expectMessage(FreeLotsResponse(evcsModel.uuid, 1))

      // ev is supposed to be departing later, but we collect it here for testing purposes
      agent ! DepartingEvsRequest(
        currentTick,
        Seq(evModel.uuid),
        service.ref,
      )
      service.expectMessage(
        DepartingEvsResponse(evcsModel.uuid, Seq(evModel))
      )

      agent ! EvFreeLotsRequest(currentTick, service.ref)
      // now, ev should be gone
      service.expectMessage(FreeLotsResponse(evcsModel.uuid, 2))
    }

    "one EV is parked, departing now" in {
      val service = createTestProbe[ServiceResponseMessage]()
      // ev is supposed to be departing at this tick
      val currentTick = 10800L

      val startingState = EvcsState(Seq(evModel), currentTick)
      val agent = spawn(testAgent(evcsModel, startingState))

      agent ! EvFreeLotsRequest(currentTick, service.ref)
      // ev should not count, since it is departing now
      service.expectMessage(FreeLotsResponse(evcsModel.uuid, 2))
    }

  }

}
