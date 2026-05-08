/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

trait ServiceStateData

object ServiceStateData {

  /** Data that is required to initialize a SimonaService
    */
  trait InitializeServiceStateData extends ServiceStateData

  trait ServiceBaseStateData extends ServiceStateData
}
