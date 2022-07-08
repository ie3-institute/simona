/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.load.LoadReference.{ActivePower, EnergyConsumption}
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID

class ProfileLoadModelSpec extends UnitSpec with TableDrivenPropertyChecks {
  "Having a profile load model" when {
    val loadInput =
      new LoadInput(
        UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
        "testLoad",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        new NodeInput(
          UUID.fromString("e5c1cde5-c161-4a4f-997f-fcf31fecbf57"),
          "TestNodeInputModel",
          OperatorInput.NO_OPERATOR_ASSIGNED,
          OperationTime.notLimited(),
          Quantities.getQuantity(1d, PowerSystemUnits.PU),
          false,
          NodeInput.DEFAULT_GEO_POSITION,
          GermanVoltageLevelUtils.LV,
          -1
        ),
        new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
        BdewStandardLoadProfile.H0,
        false,
        Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR),
        Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
        0.95
      )

    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31 23:59:00")
    val foreSeenOperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        loadInput.getOperationTime
      )
    val testingTolerance = 1e-6 // Equals to 1 W power

    "instantiating it" should {
      "deliver a proper model" in {
        forAll(
          Table(
            ("profile", "reference", "expectedsRated"),
            (
              BdewStandardLoadProfile.H0,
              ActivePower(Quantities.getQuantity(268.6, Units.WATT)),
              Quantities.getQuantity(282.74, PowerSystemUnits.VOLTAMPERE)
            ),
            (
              BdewStandardLoadProfile.H0,
              EnergyConsumption(
                Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR)
              ),
              Quantities.getQuantity(848.22, PowerSystemUnits.VOLTAMPERE)
            ),
            (
              BdewStandardLoadProfile.L0,
              ActivePower(Quantities.getQuantity(268.6, Units.WATT)),
              Quantities.getQuantity(282.74, PowerSystemUnits.VOLTAMPERE)
            ),
            (
              BdewStandardLoadProfile.L0,
              EnergyConsumption(
                Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR)
              ),
              Quantities.getQuantity(759.158, PowerSystemUnits.VOLTAMPERE)
            ),
            (
              BdewStandardLoadProfile.G0,
              ActivePower(Quantities.getQuantity(268.6, Units.WATT)),
              Quantities.getQuantity(282.74, PowerSystemUnits.VOLTAMPERE)
            ),
            (
              BdewStandardLoadProfile.G0,
              EnergyConsumption(
                Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR)
              ),
              Quantities.getQuantity(759.158, PowerSystemUnits.VOLTAMPERE)
            )
          )
        ) { (profile, reference, expectedSRated) =>
          val actual = ProfileLoadModel(
            loadInput.copy().loadprofile(profile).build(),
            foreSeenOperationInterval,
            1.0,
            reference
          )

          actual.sRated should equalWithTolerance(
            expectedSRated.to(PowerSystemUnits.MEGAVOLTAMPERE),
            testingTolerance
          )
        }
      }
    }
  }
}
