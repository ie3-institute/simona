package edu.ie3.simona.test.common.model

import edu.ie3.simona.model.participant.CalcRelevantData
import edu.ie3.simona.model.participant.SystemParticipant
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.util.scala.OperationInterval
import scala.Tuple2
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Power

import static edu.ie3.util.quantities.PowerSystemUnits.MEGAWATT

class MockParticipant extends SystemParticipant<CalcRelevantData> {

    MockParticipant(
            UUID uuid,
            String id,
            OperationInterval operationInterval,
            Double scalingFactor,
            QControl qControl,
            ComparableQuantity<Power> sRated,
            Double cosPhiRated
    ) {
        super(
                uuid,
                id,
                operationInterval,
                scalingFactor,
                qControl,
                sRated,
                cosPhiRated
        )
    }

    @Override
    ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
        return Quantities.getQuantity(0, MEGAWATT)
    }

    @Override
    FlexibilityMessage.ProvideFlexOptions determineFlexOptions(CalcRelevantData data) {
        return null
    }

    @Override
    Tuple2 handleControlledPowerChange(CalcRelevantData data, ComparableQuantity setPower) {
        return null
    }

}
