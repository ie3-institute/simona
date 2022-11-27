/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.result.system.{EvcsResult, LoadResult}
import edu.ie3.simona.agent.participant.em.EmAgent.FlexCorrespondence
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.em.EmModel.EmRelevantData
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar

import java.time.ZonedDateTime
import java.util.UUID

class EmModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with EmInputTestData
    with MockitoSugar {

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 02:00:00")

  "The em model object" should {

    val model = EmModel(
      emInput,
      modelConfig,
      simulationStartDate,
      simulationEndDate,
      PrioritizedFlexStrat // is not tested here
    )

    model.enable()

    "calculate total power based on flex options and flex control" in {
      val flexCorrespondences = Iterable(
        FlexCorrespondence(
          participantResult = Some(
            new EvcsResult(
              0L.toDateTime,
              UUID.randomUUID(),
              0.005d.asMegaWatt,
              0.0004.asMegaVar
            )
          )
        ),
        FlexCorrespondence(
          participantResult = Some(
            new LoadResult(
              0L.toDateTime,
              UUID.randomUUID(),
              0.003d.asMegaWatt,
              0.0003.asMegaVar
            )
          )
        )
      )

      val actualResult =
        model.calculatePower(
          0L,
          1d.asPu,
          ConstantState,
          EmRelevantData(flexCorrespondences)
        )

      actualResult.p should equalWithTolerance(
        0.008d.asMegaWatt,
        1e-9d
      )
      actualResult.q should equalWithTolerance(
        0.0007d.asMegaVar,
        1e-9d
      )
    }
  }

}
