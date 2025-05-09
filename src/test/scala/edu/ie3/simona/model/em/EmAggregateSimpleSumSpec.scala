/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Kilowatts

class EmAggregateSimpleSumSpec extends UnitSpec with MockitoSugar {

  "The simple sum aggregating strategy" should {

    "calculate ref, min and max power correctly" in {
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

      val actualResult = EmAggregateSimpleSum.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2),
        )
      )

      actualResult shouldBe MinMaxFlexOptions(
        Kilowatts(8.0),
        Kilowatts(2.0),
        Kilowatts(14.0),
      )
    }

  }
}
