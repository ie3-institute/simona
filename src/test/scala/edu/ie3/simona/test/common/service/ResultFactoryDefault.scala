package edu.ie3.simona.test.common.service

import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.datamodel.models.result.connector.ConnectorResult
import edu.ie3.datamodel.models.result.system.{ElectricalEnergyStorageResult, SystemParticipantResult, SystemParticipantWithHeatResult}
import edu.ie3.datamodel.models.result.thermal.ThermalUnitResult
import edu.ie3.simona.api.data.results.ResultDataFactory
class ResultFactoryDefault extends ResultDataFactory {
  @throws[Exception]
  override def convertResultToString(entity: ResultEntity): AnyRef = {
    var resultObject: String = null
    if (entity.isInstanceOf[SystemParticipantWithHeatResult]) resultObject = "{\"p\":\"" + systemParticipantWithHeatResult.getP + ",\"q\":\"" + systemParticipantWithHeatResult.getQ + ",\"qDot\":\"" + systemParticipantWithHeatResult.getqDot + "\"}"
    else if (entity.isInstanceOf[ElectricalEnergyStorageResult]) resultObject = "{\"p\":\"" + electricalEnergyStorageResult.getP + ",\"q\":\"" + electricalEnergyStorageResult.getQ + ",\"soc\":\"" + electricalEnergyStorageResult.getSoc + "\"}"
    else if (entity.isInstanceOf[ConnectorResult]) resultObject = "{\"iAMag\":\"" + connectorResult.getiAMag + ",\"iAAng\":\"" + connectorResult.getiAAng + ",\"iBMag\":\"" + connectorResult.getiBMag + ",\"iBAng\":\"" + connectorResult.getiBAng + "\"}"
    else if (entity.isInstanceOf[NodeResult]) resultObject = "{\"vMag\":\"" + nodeResult.getvMag + ",\"vAng\":\"" + nodeResult.getvAng + "\"}"
    else if (entity.isInstanceOf[ThermalUnitResult]) resultObject = "{\"qDot\":\"" + thermalUnitResult.getqDot + "\"}"
    else if (entity.isInstanceOf[SystemParticipantResult]) resultObject = "{\"p\":\"" + systemParticipantResult.getP + ",\"q\":\"" + systemParticipantResult.getQ + "\"}"
    else resultObject = "{}"
    resultObject
  }
}

