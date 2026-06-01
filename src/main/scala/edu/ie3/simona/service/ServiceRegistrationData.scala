/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

/** Container for data required for service registration.
  *
  * @param serviceTypes
  *   The service types to register for.
  * @param dataTimeType
  *   The temporal dimension of the requested data.
  */
final case class ServiceRegistrationData(
    serviceTypes: Iterable[ServiceType],
    dataTimeType: DataTimeType,
)

object ServiceRegistrationData {

  /** A [[ServiceRegistrationData]] object for situations where no data is
    * required.
    */
  lazy val noServices: ServiceRegistrationData = ServiceRegistrationData(
    serviceTypes = Iterable.empty,
    dataTimeType = DataTimeType.Current,
  )
}
