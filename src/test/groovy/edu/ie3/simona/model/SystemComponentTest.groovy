/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.OperationInterval
import scala.Option
import spock.lang.Shared
import spock.lang.Specification

import java.time.ZonedDateTime

class SystemComponentTest extends Specification {
  @Shared
  OperationTime.OperationTimeBuilder operationTimeBuilder

  def setup() {
    operationTimeBuilder = OperationTime.builder()
  }

  def "Determine the correct operation interval"(Option<ZonedDateTime> operationStart, Option<ZonedDateTime> operationEnd,  OperationInterval expected) {
    given:
    ZonedDateTime simulationStart = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    ZonedDateTime simulationEnd = TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00")

    if(operationStart.defined)
      operationTimeBuilder.withStart(operationStart.get())
    if(operationEnd.defined)
      operationTimeBuilder.withEnd(operationEnd.get())
    OperationTime operationTime = operationTimeBuilder.build()

    and:
    OperationInterval interval = SystemComponent.determineOperationInterval(simulationStart, simulationEnd, operationTime)

    expect:
    interval == expected

    where:
    operationStart | operationEnd | expected
    Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")) | Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00")) | OperationInterval.apply(0L, 86400L)
    Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00")) | Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00")) | OperationInterval.apply(86400L, 86400L)
    Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")) | Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")) | OperationInterval.apply(0L, 0L)
    Option.apply(null)                                                               | Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")) | OperationInterval.apply(0L, 0L)
    Option.apply(TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00")) | Option.apply(null)                                                                 | OperationInterval.apply(86400L, 86400L)
  }

  def "Reject an operation end, that is before the operation start"() {
    given:
    ZonedDateTime simulationStart = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    ZonedDateTime simulationEnd = TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00")
    operationTimeBuilder.withStart(TimeUtil.withDefaults.toZonedDateTime("2019-01-02 00:00:00"))
    operationTimeBuilder.withEnd(TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00"))
    OperationTime operationTime = operationTimeBuilder.build()

    when:
    SystemComponent.determineOperationInterval(simulationStart, simulationEnd, operationTime)

    then:
    def exception = thrown(InvalidParameterException.class)
    exception.message == "The defined operation end is before it's operation start."
  }
}
