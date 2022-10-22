/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.TimeUtil
import scala.Option
import spock.lang.Specification
import tech.units.indriya.quantity.Quantities

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static org.apache.commons.math3.util.FastMath.abs

class FixedFeedModelTest extends Specification {

	def fixedFeedInput = new FixedFeedInInput(
	UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
	"testFixedFeed",
	OperatorInput.NO_OPERATOR_ASSIGNED,
	OperationTime.notLimited(),
	new NodeInput(
	UUID.fromString("e5c1cde5-c161-4a4f-997f-fcf31fecbf57"),
	"TestNodeInputModel",
	OperatorInput.NO_OPERATOR_ASSIGNED,
	OperationTime.notLimited(),
	Quantities.getQuantity(1d, PU),
	false,
	NodeInput.DEFAULT_GEO_POSITION,
	GermanVoltageLevelUtils.LV,
	-1
	),
	new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
	Quantities.getQuantity(282.74d, VOLTAMPERE),
	0.95
	)
	def simulationStartDate = TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
	def simulationEndDate = TimeUtil.withDefaults.toZonedDateTime("2020-12-31 23:59:00")
	def foreSeenOperationInterval =
	SystemComponent.determineOperationInterval(
	simulationStartDate,
	simulationEndDate,
	fixedFeedInput.operationTime
	)
	def testingTolerance = 1e-6 // Equals to 1 W power

	def expectedPower = fixedFeedInput.sRated * -1 * fixedFeedInput.cosPhiRated * 1.0

	def "A fixed feed model should return approximately correct power calculations"() {
		when:
		def actualModel = new FixedFeedInModel(
				fixedFeedInput.uuid,
				fixedFeedInput.id,
				foreSeenOperationInterval,
				1.0,
				QControl.apply(fixedFeedInput.qCharacteristics),
				fixedFeedInput.sRated,
				fixedFeedInput.cosPhiRated
				)

		then:
		abs((actualModel.calculateActivePower(Option.apply(ModelState.ConstantState$), CalcRelevantData.FixedRelevantData$.MODULE$)).subtract(expectedPower).to(MEGAWATT).value.doubleValue()) < testingTolerance
	}
}
