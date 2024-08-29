/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona;

import edu.ie3.datamodel.models.result.CongestionResult;
import java.time.ZonedDateTime;
import javax.measure.quantity.Dimensionless;
import tech.units.indriya.ComparableQuantity;

public class ExtendedCongestionResult extends CongestionResult {
  private final double vMaxVal;
  private final double vMinVal;
  private final double getLineMax;

  public ExtendedCongestionResult(
      ZonedDateTime time,
      int subgrid,
      ComparableQuantity<Dimensionless> vMin,
      ComparableQuantity<Dimensionless> vMax,
      boolean voltage,
      boolean line,
      boolean transformer,
      double vMaxVal,
      double vMinVal,
      double getLineMax) {
    super(time, subgrid, vMin, vMax, voltage, line, transformer);
    this.vMaxVal = vMaxVal;
    this.vMinVal = vMinVal;
    this.getLineMax = getLineMax;
  }

  public double getvMaxVal() {
    return vMaxVal;
  }

  public double getvMinVal() {
    return vMinVal;
  }

  public double getLineMax() {
    return getLineMax;
  }

  public CongestionResult to() {
    return new CongestionResult(
        getTime(), getSubgrid(), getVMin(), getVMax(), getVoltage(), getLine(), getTransformer());
  }
}
