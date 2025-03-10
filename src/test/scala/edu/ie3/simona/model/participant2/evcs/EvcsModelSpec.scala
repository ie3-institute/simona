/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant2.ParticipantModel.OperationChangeIndicator
import edu.ie3.simona.model.participant2.evcs.EvcsModel.{
  EvcsOperatingPoint,
  EvcsState,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  ArrivingEvs,
  DepartingEvsRequest,
  DepartingEvsResponse,
  EvFreeLotsRequest,
  EvResponseMessage,
  FreeLotsResponse,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EvcsInputTestData
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.simona.test.helper.TableDrivenHelper
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Kilovars
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import squants.{Each, Energy, Power}
import squants.energy.{KilowattHours, Kilowatts}

import java.time.ZonedDateTime
import java.util.UUID

class EvcsModelSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with TableDrivenHelper
    with EvcsInputTestData {

  private implicit val energyTolerance: Energy = KilowattHours(1e-10)
  private implicit val powerTolerance: Power = Kilowatts(1e-10)

  private val dateTime: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-02T03:04:05Z")

  private def createModel(
      chargingStrategy: String,
      vehicle2Grid: Boolean = true,
  ): EvcsModel =
    EvcsModel(
      evcsInputModel.copy().v2gSupport(vehicle2Grid).build(),
      EvcsRuntimeConfig(
        chargingStrategy = chargingStrategy
      ),
    )

  "An EVCS model" should {

    "calculate new schedules correctly" when {

      "configured with max power charging" in {
        val evcsModel = createModel("maxPower")

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
        val evcsModel = createModel("constantPower")

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

      "check for correct evOperatingPoint of fully charged Ev" in {
        val evcsModel = createModel("maxPower")

        val evModel = EvModelWrapper(
          new MockEvModel(
            UUID.randomUUID(),
            "Test EV",
            5.0.asKiloWatt,
            10.0.asKiloWatt,
            20.0.asKiloWattHour,
            20.0.asKiloWattHour,
            3600,
          )
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

    "determining current state correctly" when {

      "being provided with a ChargingSchedule consisting of one entry" in {
        val evcsModel = createModel("constantPower")

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

      val evcsModel = createModel("constantPower")

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
            if (_) 1 else 0
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

    "calculate flex options correctly" when {

      "charging with constant power and allowing v2g" in {
        val evcsModel = createModel("constantPower")

        val currentTick = 7200L

        val cases = Table(
          (
            "stored1",
            "stored2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: at lower margin
          (0.0, 3.0, 15.0, 10.0, 15.0),
          // 2: mid-way full, forced charging
          (0.0, 7.5, 13.75, 10.0, 15.0),
          // 2: almost full, forced charging
          (0.0, 12.5, 11.25, 10.0, 15.0),
          // 2: full, forced charging
          (0.0, 15.0, 10.0, 10.0, 10.0),

          /* 1: at lower margin (set to 2 kWh) */
          // 2: empty
          (2.0, 0.0, 13.0, 5.0, 15.0),
          // 2: at lower margin
          (2.0, 3.0, 13.0, 0.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (2.0, 7.5, 11.75, -5.0, 15.0),
          // 2: almost full
          (2.0, 12.5, 9.25, -5.0, 15.0),
          // 2: full
          (2.0, 15.0, 8.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty, forced charging
          (5.0, 0.0, 10.0, 5.0, 15.0),
          // 2: mid-way full (set to 7.5 kWh)
          (5.0, 7.5, 8.75, -15.0, 15.0),
          // 2: almost full
          (5.0, 12.5, 6.25, -15.0, 15.0),
          // 2: full
          (5.0, 15.0, 5.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty, forced charging
          (10.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full
          (10.0, 7.5, 3.75, -15.0, 5.0),
          // 2: almost full
          (10.0, 12.5, 1.25, -15.0, 5.0),
          // 2: full
          (10.0, 15.0, 0.0, -15.0, 0.0),
        )

        forAll(cases) {
          (
              stored1,
              stored2,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            // stays one more hour
            val evA = EvModelWrapper(
              ev4.copyWith(stored1.asKiloWattHour)
            )

            // stays two more hours
            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour)
            )

            evcsModel.determineFlexOptions(
              EvcsState(
                Seq(evA, evB),
                currentTick,
              )
            ) match {
              case ProvideMinMaxFlexOptions(
                    modelUuid,
                    refPower,
                    minPower,
                    maxPower,
                  ) =>
                modelUuid shouldBe evcsModel.uuid
                refPower should approximate(Kilowatts(expectedPRef))
                minPower should approximate(Kilowatts(expectedPMin))
                maxPower should approximate(Kilowatts(expectedPMax))
            }
        }

      }

      "charging with maximum power and allowing v2g" in {
        val evcsModel = createModel("maxPower")

        val currentTick = 7200L

        val cases = Table(
          (
            "stored1",
            "stored2",
            "expectedPRef",
            "expectedPMin",
            "expectedPMax",
          ),

          /* 1: empty */
          // 2: empty
          (0.0, 0.0, 15.0, 15.0, 15.0),
          // 2: at lower margin
          (0.0, 3.0, 15.0, 10.0, 15.0),
          // 2: mid-way full, forced charging
          (0.0, 7.5, 15.0, 10.0, 15.0),
          // 2: almost full, forced charging
          (0.0, 12.5, 15.0, 10.0, 15.0),
          // 2: full
          (0.0, 15.0, 10.0, 10.0, 10.0),

          /* 1: at lower margin (set to 2 kWh) */
          // 2: empty
          (2.0, 0.0, 15.0, 5.0, 15.0),
          // 2: at lower margin
          (2.0, 3.0, 15.0, 0.0, 15.0),
          // 2: mid-way full
          (2.0, 7.5, 15.0, -5.0, 15.0),
          // 2: almost full
          (2.0, 12.5, 15.0, -5.0, 15.0),
          // 2: full
          (2.0, 15.0, 10.0, -5.0, 10.0),

          /* 1: mid-way full (set to 5 kWh) */
          // 2: empty, forced charging
          (5.0, 0.0, 15.0, 5.0, 15.0),
          // 2: mid-way full
          (5.0, 7.5, 15.0, -15.0, 15.0),
          // 2: almost full
          (5.0, 12.5, 15.0, -15.0, 15.0),
          // 2: full
          (5.0, 15.0, 10.0, -15.0, 10.0),

          /* 1: full (set to 10 kWh) */
          // 2: empty, forced charging
          (10.0, 0.0, 5.0, 5.0, 5.0),
          // 2: mid-way full
          (10.0, 7.5, 5.0, -15.0, 5.0),
          // 2: almost full
          (10.0, 12.5, 5.0, -15.0, 5.0),
          // 2: full
          (10.0, 15.0, 0.0, -15.0, 0.0),
        )

        forAll(cases) {
          (
              stored1,
              stored2,
              expectedPRef,
              expectedPMin,
              expectedPMax,
          ) =>
            val evA = EvModelWrapper(
              ev4.copyWith(stored1.asKiloWattHour)
            )

            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour).copyWithDeparture(10800L)
            )

            evcsModel.determineFlexOptions(
              EvcsState(
                Seq(evA, evB),
                currentTick,
              )
            ) match {
              case ProvideMinMaxFlexOptions(
                    modelUuid,
                    refPower,
                    minPower,
                    maxPower,
                  ) =>
                modelUuid shouldBe evcsModel.uuid
                refPower should approximate(Kilowatts(expectedPRef))
                minPower should approximate(Kilowatts(expectedPMin))
                maxPower should approximate(Kilowatts(expectedPMax))
            }
        }

      }

      "disallowing v2g" in {
        val evcsModel = createModel("constantPower", vehicle2Grid = false)

        val currentTick = 7200L

        val ev1 = EvModelWrapper(
          ev4.copyWith(5.0.asKiloWattHour)
        )

        evcsModel.determineFlexOptions(
          EvcsState(
            Seq(ev1),
            currentTick,
          )
        ) match {
          case ProvideMinMaxFlexOptions(
                modelUuid,
                refPower,
                minPower,
                maxPower,
              ) =>
            modelUuid shouldBe evcsModel.uuid
            refPower should approximate(Kilowatts(5.0)) // one hour left
            minPower should approximate(Kilowatts(0d)) // no v2g allowed!
            maxPower should approximate(ev1.pRatedAc)
        }

      }

    }

    "handle power control correctly" when {
      val evcsModel = createModel("constantPower")

      "dealing with two evs" in {
        val currentTick = 3600L

        val cases = Table(
          (
            "stored1",
            "stored2",
            "setPower",
            "expPower1",
            "expPower2",
            "expNextActivation",
            "expNextTick",
          ),

          /* setPower is 0 kWh */
          (0.0, 0.0, 0.0, 0.0, 0.0, false, N),
          (10.0, 5.0, 0.0, 0.0, 0.0, false, N),
          (5.0, 15.0, 0.0, 0.0, 0.0, false, N),
          (10.0, 15.0, 0.0, 0.0, 0.0, false, N),

          /* setPower is positive (charging) */
          (0.0, 0.0, 4.0, 2.0, 2.0, true, S(7200L)),
          (5.0, 0.0, 4.0, 0.0, 4.0, true, S(6300L)),
          (0.0, 7.5, 4.0, 4.0, 0.0, true, S(5400L)),
          (9.0, 0.0, 4.0, 0.0, 4.0, true, S(6300L)),
          (5.0, 14.0, 4.0, 2.0, 2.0, false, S(5400L)),
          (9.0, 14.0, 4.0, 2.0, 2.0, false, S(5400L)),
          (10.0, 14.0, 4.0, 0.0, 4.0, false, S(4500L)),
          (6.0, 15.0, 4.0, 4.0, 0.0, false, S(7200L)),

          /* setPower is set to > (ev2 * 2) (charging) */
          (0.0, 0.0, 13.0, 8.0, 5.0, true, S(4500L)),
          (7.0, 0.0, 11.0, 6.0, 5.0, true, S(5400L)),
          (0.0, 5.0, 15.0, 10.0, 5.0, true, S(4320L)),
          (0.0, 12.5, 15.0, 10.0, 5.0, true, S(4320L)),
          (0.0, 0.0, 15.0, 10.0, 5.0, true, S(4320L)),
          (5.0, 7.5, 15.0, 10.0, 5.0, false, S(5400L)),

          /* setPower is negative (discharging) */
          (10.0, 15.0, -4.0, -2.0, -2.0, true, S(7200L)),
          (5.0, 15.0, -4.0, -2.0, -2.0, true, S(7200L)),
          (10.0, 7.5, -4.0, -2.0, -2.0, true, S(7200L)),
          (3.0, 15.0, -4.0, -2.0, -2.0, true, S(5400L)),
          (5.0, 4.0, -4.0, -2.0, -2.0, false, S(5400L)),
          (3.0, 4.0, -4.0, -2.0, -2.0, false, S(5400L)),
          (0.0, 4.0, -4.0, 0.0, -4.0, false, S(4500L)),
          (6.0, 0.0, -4.0, -4.0, 0.0, false, S(7200L)),

          /* setPower is set to > (ev2 * 2) (discharging) */
          (10.0, 15.0, -13.0, -8.0, -5.0, true, S(7200L)),
          (5.0, 15.0, -11.0, -6.0, -5.0, true, S(5400L)),
          (10.0, 8.0, -15.0, -10.0, -5.0, true, S(6480L)),
          (10.0, 5.5, -15.0, -10.0, -5.0, true, S(5400L)),
          (10.0, 15.0, -15.0, -10.0, -5.0, true, S(6480L)),
          (7.0, 10.5, -15.0, -10.0, -5.0, false, S(5400L)),
        )

        forAll(cases) {
          (
              stored1: Double,
              stored2: Double,
              setPower: Double,
              expPower1: Double,
              expPower2: Double,
              expNextActivation: Boolean,
              expNextTick: Option[Long],
          ) =>
            val evA = EvModelWrapper(
              ev4.copyWith(stored1.asKiloWattHour).copyWithDeparture(7200L)
            )

            val evB = EvModelWrapper(
              ev5.copyWith(stored2.asKiloWattHour).copyWithDeparture(10800L)
            )

            evcsModel.determineOperatingPoint(
              EvcsState(
                Seq(evA, evB),
                currentTick,
              ),
              Kilowatts(setPower),
            ) match {
              case (
                    EvcsOperatingPoint(evOperatingPoints),
                    OperationChangeIndicator(
                      actualNextActivation,
                      actualNextTick,
                    ),
                  ) =>
                evOperatingPoints
                  .get(evA.uuid)
                  .value shouldBe Kilowatts(expPower1)
                evOperatingPoints
                  .get(evB.uuid)
                  .value shouldBe Kilowatts(expPower2)

                actualNextActivation shouldBe expNextActivation
                actualNextTick shouldBe expNextTick
            }
        }

      }

    }

    "handle arrivals correctly" in {
      val evcsModel = createModel("maxPower")

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
      val evcsModel = createModel("constantPower")

      val evModel = EvModelWrapper(
        ev3.copyWith(5.0.asKiloWattHour)
      )

      def testAgent(
          model: EvcsModel,
          state: EvcsState,
      ): Behavior[ParticipantAgent.Request] = Behaviors.receivePartial {
        case (ctx, request: ParticipantRequest) =>
          val newState = model.handleRequest(
            state,
            ctx,
            request,
          )

          testAgent(model, newState)
      }

      "no EVs are parked" in {
        val service = createTestProbe[EvResponseMessage]()
        val currentTick = 0L

        val startingState = EvcsState(Seq.empty, currentTick)
        val agent = spawn(testAgent(evcsModel, startingState))

        agent ! EvFreeLotsRequest(currentTick, service.ref.toClassic)
        service.expectMessage(FreeLotsResponse(evcsModel.uuid, 2))
      }

      "one EV is parked, departing later" in {
        val service = createTestProbe[EvResponseMessage]()
        val currentTick = 0L

        val startingState = EvcsState(Seq(evModel), currentTick)
        val agent = spawn(testAgent(evcsModel, startingState))

        agent ! EvFreeLotsRequest(currentTick, service.ref.toClassic)
        service.expectMessage(FreeLotsResponse(evcsModel.uuid, 1))

        // ev is supposed to be departing later, but we collect it here for testing purposes
        agent ! DepartingEvsRequest(
          currentTick,
          Seq(evModel.uuid),
          service.ref.toClassic,
        )
        service.expectMessage(
          DepartingEvsResponse(evcsModel.uuid, Seq(evModel))
        )

        agent ! EvFreeLotsRequest(currentTick, service.ref.toClassic)
        // now, ev should be gone
        service.expectMessage(FreeLotsResponse(evcsModel.uuid, 2))
      }

      "one EV is parked, departing now" in {
        val service = createTestProbe[EvResponseMessage]()
        // ev is supposed to be departing at this tick
        val currentTick = 10800L

        val startingState = EvcsState(Seq(evModel), currentTick)
        val agent = spawn(testAgent(evcsModel, startingState))

        agent ! EvFreeLotsRequest(currentTick, service.ref.toClassic)
        // ev should not count, since it is departing now
        service.expectMessage(FreeLotsResponse(evcsModel.uuid, 2))
      }

    }

  }

}
