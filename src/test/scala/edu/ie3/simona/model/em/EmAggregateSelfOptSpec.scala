/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.system.{PvInput, SystemParticipantInput}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Kilowatts

import java.util.UUID

class EmAggregateSelfOptSpec extends UnitSpec with MockitoSugar {

  "The self-optimizing aggregating strategy with PV flex" should {
    val strat = EmAggregateSelfOpt(curtailRegenerative = true)

    "pick 0kW if possible" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        ref = Kilowatts(2.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
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
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        ref = Kilowatts(6.0),
        min = Kilowatts(4.0),
        max = Kilowatts(12.0),
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
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
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        ref = Kilowatts(-1.0),
        min = Kilowatts(-10.0),
        max = Kilowatts(-1.0),
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
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
    val strat = EmAggregateSelfOpt(curtailRegenerative = false)

    "exclude PV max power when normally picking 0kW as target" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        ref = Kilowatts(2.0),
        min = Kilowatts(-1.0),
        max = Kilowatts(4.0),
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
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
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        ref = Kilowatts(-1.0),
        min = Kilowatts(-10.0),
        max = Kilowatts(-1.0),
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
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
}
