/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result;

import edu.ie3.datamodel.exceptions.EntityProcessorException;
import edu.ie3.datamodel.io.processor.result.ResultEntityProcessor;
import edu.ie3.datamodel.models.OperationTime;
import edu.ie3.datamodel.models.UniqueEntity;
import edu.ie3.datamodel.models.input.OperatorInput;
import edu.ie3.datamodel.models.input.system.characteristic.CharacteristicInput;
import edu.ie3.datamodel.models.profile.LoadProfile;
import edu.ie3.datamodel.models.result.CongestionResult;
import edu.ie3.datamodel.models.result.ResultEntity;
import edu.ie3.datamodel.models.voltagelevels.VoltageLevel;
import edu.ie3.datamodel.utils.Try;
import java.lang.reflect.Method;
import java.time.ZonedDateTime;
import java.util.Optional;
import javax.measure.Quantity;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.geojson.GeoJsonWriter;

public class FixedResultEntityProcessor extends ResultEntityProcessor {
  public FixedResultEntityProcessor(Class<? extends ResultEntity> registeredClass)
      throws EntityProcessorException {
    super(registeredClass);
  }

  private static final GeoJsonWriter geoJsonWriter = new GeoJsonWriter();

  protected String processMethodResult(Object methodReturnObject, Method method, String fieldName)
      throws EntityProcessorException {

    StringBuilder resultStringBuilder = new StringBuilder();

    switch (method.getReturnType().getSimpleName()) {
      // primitives (Boolean, Character, Byte, Short, Integer, Long, Float, Double, String,
      case "UUID",
              "boolean",
              "int",
              "double",
              "String",
              "DayOfWeek",
              "Season",
              "ChargingPointType",
              "EvcsLocationType" ->
          resultStringBuilder.append(methodReturnObject.toString());
      case "Quantity", "ComparableQuantity" ->
          resultStringBuilder.append(handleQuantity((Quantity<?>) methodReturnObject, fieldName));
      case "Optional" ->
          // only quantity optionals are expected here!
          // if optional and present, unpack value and call this method again, if not present return
          // an empty string as by convention null == missing value == "" when persisting data
          resultStringBuilder.append(
              ((Optional<?>) methodReturnObject)
                  .map(
                      o -> {
                        if (o instanceof Quantity<?> quantity) {
                          return Try.of(
                              () -> handleQuantity(quantity, fieldName),
                              EntityProcessorException.class);
                        } else if (o instanceof UniqueEntity entity) {
                          return Try.of(entity::getUuid, EntityProcessorException.class);
                        } else {
                          return Try.Failure.of(
                              new EntityProcessorException(
                                  "Handling of "
                                      + o.getClass().getSimpleName()
                                      + ".class instance wrapped into Optional is currently not supported by entity processors!"));
                        }
                      })
                  .orElse(Try.Success.of("")) // (in case of empty optional)
                  .getOrThrow());
      case "ZonedDateTime" ->
          resultStringBuilder.append(processZonedDateTime((ZonedDateTime) methodReturnObject));
      case "OperationTime" ->
          resultStringBuilder.append(
              processOperationTime((OperationTime) methodReturnObject, fieldName));
      case "VoltageLevel" ->
          resultStringBuilder.append(
              processVoltageLevel((VoltageLevel) methodReturnObject, fieldName));
      case "Point", "LineString" ->
          resultStringBuilder.append(geoJsonWriter.write((Geometry) methodReturnObject));
      case "LoadProfile", "BdewStandardLoadProfile", "RandomLoadProfile" ->
          resultStringBuilder.append(((LoadProfile) methodReturnObject).getKey());
      case "AssetTypeInput",
              "BmTypeInput",
              "ChpTypeInput",
              "EvTypeInput",
              "HpTypeInput",
              "LineTypeInput",
              "LineInput",
              "NodeInput",
              "StorageTypeInput",
              "SystemParticipantInput",
              "ThermalBusInput",
              "ThermalStorageInput",
              "TimeSeries",
              "Transformer2WTypeInput",
              "Transformer3WTypeInput",
              "WecTypeInput",
              "EmInput" ->
          resultStringBuilder.append(((UniqueEntity) methodReturnObject).getUuid());
      case "OperatorInput" ->
          resultStringBuilder.append(
              ((OperatorInput) methodReturnObject).getId().equalsIgnoreCase("NO_OPERATOR_ASSIGNED")
                  ? ""
                  : ((OperatorInput) methodReturnObject).getUuid());
      case "EvCharacteristicInput",
              "OlmCharacteristicInput",
              "WecCharacteristicInput",
              "CosPhiFixed",
              "CosPhiP",
              "QV",
              "ReactivePowerCharacteristic",
              "CharacteristicInput" ->
          resultStringBuilder.append(((CharacteristicInput<?, ?>) methodReturnObject).serialize());
      case "InputModelType" ->
          resultStringBuilder.append(((CongestionResult.InputModelType) methodReturnObject).type);
      default ->
          throw new EntityProcessorException(
              "Unable to process value for attribute/field '"
                  + fieldName
                  + "' and method return type '"
                  + method.getReturnType().getSimpleName()
                  + "' for method with name '"
                  + method.getName()
                  + "' in in entity model "
                  + getRegisteredClass().getSimpleName()
                  + ".class.");
    }

    return resultStringBuilder.toString();
  }
}
