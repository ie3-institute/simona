/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model;

import edu.ie3.simona.api.data.ev.model.EvModel;
import edu.ie3.util.quantities.PowerSystemUnits;
import java.util.Objects;
import java.util.UUID;
import javax.measure.quantity.Energy;
import javax.measure.quantity.Power;
import tech.units.indriya.ComparableQuantity;
import tech.units.indriya.quantity.Quantities;

public class MockEvModel implements EvModel {
  private final UUID uuid;
  private final String id;
  private final ComparableQuantity<Power> sRatedAC;
  private final ComparableQuantity<Power> sRatedDC;
  private final ComparableQuantity<Energy> eStorage;
  private final ComparableQuantity<Energy> storedEnergy;
  private final Long departureTick;

  public MockEvModel(
      UUID uuid,
      String id,
      ComparableQuantity<Power> sRatedAC,
      ComparableQuantity<Power> sRatedDC,
      ComparableQuantity<Energy> eStorage,
      ComparableQuantity<Energy> storedEnergy,
      Long departureTick) {
    this.uuid = uuid;
    this.id = id;
    this.sRatedAC = sRatedAC;
    this.sRatedDC = sRatedDC;
    this.eStorage = eStorage;
    this.storedEnergy = storedEnergy;
    this.departureTick = departureTick;
  }

  public MockEvModel(
      UUID uuid,
      String id,
      ComparableQuantity<Power> sRatedAC,
      ComparableQuantity<Power> sRatedDC,
      ComparableQuantity<Energy> eStorage,
      Long departureTick) {
    this.uuid = uuid;
    this.id = id;
    this.sRatedAC = sRatedAC;
    this.sRatedDC = sRatedDC;
    this.eStorage = eStorage;
    this.storedEnergy = Quantities.getQuantity(0d, PowerSystemUnits.KILOWATTHOUR);
    this.departureTick = departureTick;
  }

  @Override
  public UUID getUuid() {
    return uuid;
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public ComparableQuantity<Power> getPRatedAC() {
    return sRatedAC;
  }

  @Override
  public ComparableQuantity<Power> getPRatedDC() {
    return sRatedDC;
  }

  @Override
  public ComparableQuantity<Energy> getEStorage() {
    return eStorage;
  }

  @Override
  public ComparableQuantity<Energy> getStoredEnergy() {
    return storedEnergy;
  }

  @Override
  public Long getDepartureTick() {
    return departureTick;
  }

  @Override
  public MockEvModel copyWith(ComparableQuantity<Energy> newStoredEnergy) {
    return new MockEvModel(uuid, id, sRatedAC, sRatedDC, eStorage, newStoredEnergy, departureTick);
  }

  public MockEvModel copyWithDeparture(Long departureTick) {
    return new MockEvModel(uuid, id, sRatedAC, sRatedDC, eStorage, storedEnergy, departureTick);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MockEvModel that = (MockEvModel) o;
    return uuid.equals(that.uuid)
        && id.equals(that.id)
        && sRatedAC.equals(that.sRatedAC)
        && sRatedDC.equals(that.sRatedDC)
        && eStorage.equals(that.eStorage)
        && storedEnergy.equals(that.storedEnergy)
        && departureTick.equals(that.departureTick);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, id, sRatedAC, sRatedDC, eStorage, storedEnergy, departureTick);
  }
}
