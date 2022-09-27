package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.type.StorageTypeInput
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.util.TimeUtil
import spock.lang.Shared
import spock.lang.Specification

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static edu.ie3.util.quantities.QuantityUtil.equals
import static tech.units.indriya.quantity.Quantities.getQuantity

class StorageModelTest extends Specification {

    @Shared
    StorageInput inputModel
    @Shared
    static final Double TOLERANCE = 1e-10

    def setupSpec() {
        def nodeInput = new NodeInput(
                UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
                "NodeInput",
                OperatorInput.NO_OPERATOR_ASSIGNED,
                OperationTime.notLimited(),
                getQuantity(1d, PU),
                false,
                NodeInput.DEFAULT_GEO_POSITION,
                GermanVoltageLevelUtils.LV,
                -1)

        def typeInput = new StorageTypeInput(
                UUID.fromString("fbee4995-24dd-45e4-9c85-7d986fe99ff3"),
                "Test_StorageTypeInput",
                getQuantity(100d, EURO),
                getQuantity(101d, EURO_PER_MEGAWATTHOUR),
                getQuantity(100d, KILOWATTHOUR),
                getQuantity(13d, KILOVOLTAMPERE),
                0.997,
                getQuantity(10d, KILOWATT),
                getQuantity(0.03, PU_PER_HOUR),
                getQuantity(0.9, PU),
                getQuantity(20d, PERCENT),
                getQuantity(43800.0, HOUR),
                100000
        )

        inputModel = new StorageInput(
                UUID.randomUUID(),
                "Test_StorageInput",
                new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
                OperationTime.notLimited(),
                nodeInput,
                CosPhiFixed.CONSTANT_CHARACTERISTIC,
                typeInput
        )
    }

    def buildStorageModel() {
        return StorageModel.apply(inputModel, 1,
                TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00"),
                TimeUtil.withDefaults.toZonedDateTime("2020-01-01 01:00:00"))
    }

    def "Calculate flex options"() {
        given:
        def storageModel = buildStorageModel()
        def startTick = 3600L
        def data = new StorageModel.StorageRelevantData(startTick + timeDelta)
        def oldState = new StorageModel.StorageState(
                getQuantity(lastStored, KILOWATTHOUR),
                getQuantity(lastPower, KILOWATT),
                startTick
        )

        when:
        def result = (FlexibilityMessage.ProvideMinMaxFlexOptions) storageModel.determineFlexOptions(data, oldState)

        then:
        equals(result.referencePower(), getQuantity(pRef, KILOWATT), TOLERANCE)
        equals(result.minPower(), getQuantity(pMin, KILOWATT), TOLERANCE)
        equals(result.maxPower(), getQuantity(pMax, KILOWATT), TOLERANCE)

        where:
        lastStored | lastPower | timeDelta || pRef | pMin | pMax
        // UNCHANGED STATE
        // completely empty
        0          | 0         | 1         || 0    | 0    | 10
        // at lowest allowed charge
        20         | 0         | 1         || 0    | 0    | 10
        // at a tiny bit above lowest allowed charge
        20.011     | 0         | 1         || 0    | -10  | 10
        // at mid-level charge
        60         | 0         | 1         || 0    | -10  | 10
        // almost fully charged
        99.989     | 0         | 1         || 0    | -10  | 10
        // fully charged
        100        | 0         | 1         || 0    | -10  | 0
        // CHANGED STATE
        // discharged to lowest allowed charge
        30         | -10       | 3600      || 0    | 0    | 10
        // almost discharged to lowest allowed charge
        30         | -10       | 3590      || 0    | -10  | 10
        // charged to mid-level charge
        50         | 10        | 3600      || 0    | -10  | 10
        // discharged to mid-level charge
        70         | -10       | 3600      || 0    | -10  | 10
        // almost fully charged
        95         | 4.98      | 3600      || 0    | -10  | 10
        // fully charged
        95         | 5         | 3600      || 0    | -10  | 0
    }

    def "Handle controlled power change"() {
        given:
        def storageModel = buildStorageModel()
        def startTick = 3600L
        def data = new StorageModel.StorageRelevantData(startTick + 1)
        def oldState = new StorageModel.StorageState(
                getQuantity(lastStored, KILOWATTHOUR),
                getQuantity(0d, KILOWATT),
                startTick
        )

        when:
        def result = storageModel.handleControlledPowerChange(
                data,
                oldState,
                getQuantity(setPower, KILOWATT)
        )

        then:
        equals(result._1.chargingPower(), getQuantity(expPower, KILOWATT), TOLERANCE)
        result._1.tick() == startTick + 1
        equals(result._1.storedEnergy(), getQuantity(lastStored, KILOWATTHOUR), TOLERANCE)
        def flexChangeIndication = result._2
        flexChangeIndication.changesAtTick().defined == expScheduled
        flexChangeIndication.changesAtTick().map(x -> x == startTick + 1 + expDelta).getOrElse( _ -> true)
        flexChangeIndication.changesAtNextActivation() == expActiveNext

        where:
        lastStored | setPower || expPower | expActiveNext | expScheduled | expDelta
        // no power
        0          | 0        || 0        | false         | false        | 0
        50         | 0        || 0        | false         | false        | 0
        100        | 0        || 0        | false         | false        | 0
        // charging on empty
        0          | 1        || 0.9      | true          | true         | 100*3600/0.9
        0          | 2.5      || 2.25     | true          | true         | 40*3600/0.9
        0          | 5        || 4.5      | true          | true         | 20*3600/0.9
        0          | 10       || 9        | true          | true         | 10*3600/0.9
        // charging on half full
        50         | 5        || 4.5      | false         | true         | 10*3600/0.9
        50         | 10       || 9        | false         | true         | 5*3600/0.9
        // discharging on half full
        50         | -5       || -4.5     | false         | true         | 6*3600/0.9
        50         | -10      || -9       | false         | true         | 3*3600/0.9
        // discharging on full
        100        | -5       || -4.5     | true          | true         | 16*3600/0.9
        100        | -10      || -9       | true          | true         | 8*3600/0.9
    }

}