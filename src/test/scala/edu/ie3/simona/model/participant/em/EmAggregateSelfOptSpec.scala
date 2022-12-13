/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatestplus.mockito.MockitoSugar

import java.util.UUID

class EmAggregateSelfOptSpec extends UnitSpec with MockitoSugar {

  "The self-optimizing aggregating strategy" should {

    "pick 0kW if possible" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = 6d.asKiloWatt,
        minPower = (-1d).asKiloWatt,
        maxPower = 12d.asKiloWatt
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = 2d.asKiloWatt,
        minPower = (-2d).asKiloWatt,
        maxPower = 2d.asKiloWatt
      )

      val actualResult = EmAggregateSelfOpt.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2)
        )
      )

      actualResult shouldBe (
        0d.asKiloWatt,
        (-3d).asKiloWatt,
        14d.asKiloWatt
      )
    }

    "pick minSum if minSum > 0kW" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = 6d.asKiloWatt,
        minPower = 4d.asKiloWatt,
        maxPower = 12d.asKiloWatt
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = 2d.asKiloWatt,
        minPower = (-2d).asKiloWatt,
        maxPower = 2d.asKiloWatt
      )

      val actualResult = EmAggregateSelfOpt.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2)
        )
      )

      actualResult shouldBe (
        2d.asKiloWatt,
        2d.asKiloWatt,
        14d.asKiloWatt
      )
    }

    "pick maxSum if maxSum < 0kW" in {
      val flexOptions1 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = 2d.asKiloWatt,
        minPower = (-6d).asKiloWatt,
        maxPower = 2d.asKiloWatt
      )

      val flexOptions2 = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        referencePower = (-6d).asKiloWatt,
        minPower = (-10d).asKiloWatt,
        maxPower = (-6d).asKiloWatt
      )

      val actualResult = EmAggregateSelfOpt.aggregateFlexOptions(
        Iterable(
          (mock[SystemParticipantInput], flexOptions1),
          (mock[SystemParticipantInput], flexOptions2)
        )
      )

      actualResult shouldBe (
        (-4d).asKiloWatt,
        (-16d).asKiloWatt,
        (-4d).asKiloWatt
      )
    }
  }
}
