/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.exceptions

/** Abstract class pattern for all exceptions around the weather service
  *
  * @param msg
  *   Message to prompt to the end user
  * @param cause
  *   Cause of this specific exception
  */
abstract class WeatherServiceException(
    private val msg: String = "",
    private val cause: Throwable = None.orNull,
) extends Exception(msg, cause)

object WeatherServiceException {

  /** Exception to be thrown, if initialization of the
    * [[edu.ie3.simona.service.weather.WeatherService]] fails
    *
    * @param msg
    *   Message to prompt to the end user
    * @param cause
    *   Cause of this specific exception
    */
  final case class WeatherServiceInitializationException(
      private val msg: String = "",
      private val cause: Throwable = None.orNull,
  ) extends WeatherServiceException(msg, cause)

  /** Exception to be thrown, if looking up of weather fails
    *
    * @param msg
    *   Message to prompt to the end user
    * @param cause
    *   Cause of this specific exception
    */
  final case class WeatherLookupException(
      private val msg: String = "",
      private val cause: Throwable = None.orNull,
  ) extends WeatherServiceException(msg, cause)

  /** Exception to be thrown if the registration of the an agent fails
    * @param msg
    *   Message to prompt to the end user
    * @param cause
    *   Cause of this specific exception
    */
  final case class InvalidRegistrationRequestException(
      private val msg: String = "",
      private val cause: Throwable = None.orNull,
  ) extends WeatherServiceException(msg, cause)

}
