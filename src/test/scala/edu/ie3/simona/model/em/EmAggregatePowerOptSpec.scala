/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.system.{PvInput, SystemParticipantInput}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Kilowatts

class EmAggregatePowerOptSpec extends UnitSpec with MockitoSugar {

  "The aggregating strategy overall" should {
    val strat = EmAggregatePowerOpt(curtailRegenerative = true)

    "work with single flex options" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(2.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1)
        )
      )

      actualResult shouldBe (
        Kilowatts(0.0),
        Kilowatts(-1.0),
        Kilowatts(4.0)
      )
    }
    "work as expected at zero flexibility" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(5.0),
        min = Kilowatts(5.0),
        max = Kilowatts(5.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1)
        )
      )

      actualResult shouldBe (
        Kilowatts(5.0),
        Kilowatts(5.0),
        Kilowatts(5.0)
      )
    }
  }

  "The self-optimizing aggregating strategy with PV flex" should {
    val strat = EmAggregatePowerOpt(curtailRegenerative = true)

    "pick 0kW if possible" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(2.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(0.0),
        Kilowatts(-7.0),
        Kilowatts(4.0)
      )
    }

    "pick minSum if minSum > 0kW" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(6.0),
        min = Kilowatts(4.0),
        max = Kilowatts(12.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(2.0),
        min = Kilowatts(-2.0),
        max = Kilowatts(2.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(2.0),
        Kilowatts(2.0),
        Kilowatts(14.0)
      )
    }

    "pick maxSum if maxSum < 0kW" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(-1.0),
        min = Kilowatts(-10.0),
        max = Kilowatts(-1.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-1.0),
        Kilowatts(-16.0),
        Kilowatts(-1.0)
      )
    }
  }

  "The self-optimizing aggregating strategy without PV flex" should {
    val strat = EmAggregatePowerOpt(curtailRegenerative = false)

    "exclude PV max power when normally picking 0kW as target" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(2.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-2.0),
        Kilowatts(-7.0),
        Kilowatts(4.0)
      )
    }

    "exclude PV max power when normally picking maxSum as target" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(-1.0),
        min = Kilowatts(-10.0),
        max = Kilowatts(-1.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-7.0),
        Kilowatts(-16.0),
        Kilowatts(-1.0)
      )
    }
  }

  "The power target aggregating strategy with PV flex" should {
    val powerTarget = Kilowatts(3d)
    val strat = EmAggregatePowerOpt(powerTarget, curtailRegenerative = true)

    "pick closed possible power if power target is not possible" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(5.0),
        min = Kilowatts(4.0),
        max = Kilowatts(6.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(0.0),
        min = Kilowatts(0.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(4.0),
        Kilowatts(4.0),
        Kilowatts(6.0)
      )
    }

    "use min flex of to stay inside inside power target limits" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(10.0),
        min = Kilowatts(9.0),
        max = Kilowatts(11.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(3.0),
        Kilowatts(3.0),
        Kilowatts(11.0)
      )
    }

    "use ref and stay inside inside power target limits" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(8.0),
        min = Kilowatts(7.0),
        max = Kilowatts(11.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(2.0),
        Kilowatts(1.0),
        Kilowatts(11.0)
      )
    }

    "pick power target if possible" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(9.0),
        min = Kilowatts(8.0),
        max = Kilowatts(10.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(3.0),
        Kilowatts(2.0),
        Kilowatts(10.0)
      )
    }

    "pick reference inside power target limits if possible" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(9.0),
        min = Kilowatts(8.0),
        max = Kilowatts(10.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-7.0),
        min = Kilowatts(-7.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(2.0),
        Kilowatts(1.0),
        Kilowatts(10.0)
      )
    }

    "stay inside inside power target limits and not reduce renewable generation" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(9.0),
        min = Kilowatts(8.0),
        max = Kilowatts(10.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-12.0),
        min = Kilowatts(-12.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-3.0),
        Kilowatts(-4.0),
        Kilowatts(10.0)
      )
    }

    "use max flex of to stay inside inside power target limits and not reduce renewable generation" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(1.0),
        min = Kilowatts(0.0),
        max = Kilowatts(5.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-8.0),
        min = Kilowatts(-8.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-3.0),
        Kilowatts(-8.0),
        Kilowatts(5.0)
      )
    }

    "pick 3kW if possible" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(10.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(12.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(3.0),
        Kilowatts(-7.0),
        Kilowatts(12.0)
      )
    }

    "pick -3kW if possible" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(2.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-3.0),
        Kilowatts(-7.0),
        Kilowatts(4.0)
      )
    }

    "pick reference power (positive) if inside the power target limits" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(8.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(10.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(2.0),
        Kilowatts(-7.0),
        Kilowatts(10.0)
      )
    }

    "pick (negative) reference power if inside the power target limits" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(4.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-2.0),
        Kilowatts(-7.0),
        Kilowatts(4.0)
      )
    }

    "pick minSum if minSum > 0kW" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(6.0),
        min = Kilowatts(4.0),
        max = Kilowatts(12.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(2.0),
        min = Kilowatts(-2.0),
        max = Kilowatts(2.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(3.0),
        Kilowatts(2.0),
        Kilowatts(14.0)
      )
    }

    "pick maxSum if maxSum < 0kW" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(-1.0),
        min = Kilowatts(-10.0),
        max = Kilowatts(-1.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-3.0),
        Kilowatts(-16.0),
        Kilowatts(-1.0)
      )
    }
  }

  "The power target aggregating strategy without PV flex" should {
    val powerTarget = Kilowatts(3d)
    val strat = EmAggregatePowerOpt(powerTarget, curtailRegenerative = false)

    "pick min power to get closed possible to power target if it cannot reached" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(11.0),
        min = Kilowatts(10.0),
        max = Kilowatts(12.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(4.0),
        Kilowatts(4.0),
        Kilowatts(12.0)
      )
    }

    "pick reference power if power target can be reached" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(9.0),
        min = Kilowatts(8.0),
        max = Kilowatts(12.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(3.0),
        Kilowatts(2.0),
        Kilowatts(12.0)
      )
    }

    "pick max power to get closed possible to power target if it cannot reached" in {
      val flexOptions1 = MinMaxFlexOptions(
        ref = Kilowatts(1.0),
        min = Kilowatts(0.0),
        max = Kilowatts(2.0),
      )

      val flexOptions2 = MinMaxFlexOptions(
        ref = Kilowatts(-6.0),
        min = Kilowatts(-6.0),
        max = Kilowatts(0.0),
      )

      val actualResult = strat.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[PvInput], flexOptions2),
        )
      )

      actualResult shouldBe (
        Kilowatts(-4.0),
        Kilowatts(-6.0),
        Kilowatts(2.0)
      )
    }
  }
}
