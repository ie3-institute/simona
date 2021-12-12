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
  private final ComparableQuantity<Power> sRated;
  private final ComparableQuantity<Energy> eStorage;
  private final ComparableQuantity<Energy> storedEnergy;

  public MockEvModel(
      UUID uuid,
      String id,
      ComparableQuantity<Power> sRated,
      ComparableQuantity<Energy> eStorage,
      ComparableQuantity<Energy> storedEnergy) {
    this.uuid = uuid;
    this.id = id;
    this.sRated = sRated;
    this.eStorage = eStorage;
    this.storedEnergy = storedEnergy;
  }

  public MockEvModel(
      UUID uuid, String id, ComparableQuantity<Power> sRated, ComparableQuantity<Energy> eStorage) {
    this.uuid = uuid;
    this.id = id;
    this.sRated = sRated;
    this.eStorage = eStorage;
    this.storedEnergy = Quantities.getQuantity(0d, PowerSystemUnits.KILOWATTHOUR);
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
  public ComparableQuantity<Power> getSRatedAC() {
    return sRated;
  }

  @Override
  public ComparableQuantity<Power> getSRatedDC() {
    return sRated;
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
    return 0L;
  }

  @Override
  public MockEvModel copyWith(ComparableQuantity<Energy> newStoredEnergy) {
    return new MockEvModel(uuid, id, sRated, eStorage, newStoredEnergy);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MockEvModel that = (MockEvModel) o;
    return uuid.equals(that.uuid)
        && id.equals(that.id)
        && sRated.equals(that.sRated)
        && eStorage.equals(that.eStorage)
        && storedEnergy.equals(that.storedEnergy);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, id, sRated, eStorage, storedEnergy);
  }
}
