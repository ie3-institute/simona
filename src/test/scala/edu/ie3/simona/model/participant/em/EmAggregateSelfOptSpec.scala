/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Kilowatts

import java.util.UUID

class EmAggregateSelfOptSpec extends UnitSpec with MockitoSugar {

  "The self-optimizing aggregating strategy" should {

    "pick 0kW if possible" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = Kilowatts(6.0),
        minPower = Kilowatts(-1.0),
        maxPower = Kilowatts(12.0)
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = Kilowatts(2.0),
        minPower = Kilowatts(-2.0),
        maxPower = Kilowatts(2.0)
      )

      val actualResult = EmAggregateSelfOpt.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2)
        )
      )

      actualResult shouldBe (
        Kilowatts(0.0),
        Kilowatts(-3.0),
        Kilowatts(14.0)
      )
    }

    "pick minSum if minSum > 0kW" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = Kilowatts(6.0),
        minPower = Kilowatts(4.0),
        maxPower = Kilowatts(12.0)
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = Kilowatts(2.0),
        minPower = Kilowatts(-2.0),
        maxPower = Kilowatts(2.0)
      )

      val actualResult = EmAggregateSelfOpt.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2)
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
        referencePower = Kilowatts(2.0),
        minPower = Kilowatts(-6.0),
        maxPower = Kilowatts(2.0)
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = Kilowatts(-6.0),
        minPower = Kilowatts(-10.0),
        maxPower = Kilowatts(-6.0)
      )

      val actualResult = EmAggregateSelfOpt.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2)
        )
      )

      actualResult shouldBe (
        Kilowatts(-4.0),
        Kilowatts(-16.0),
        Kilowatts(-4.0)
      )
    }
  }
}
