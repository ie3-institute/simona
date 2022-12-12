/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.helper

trait TableDrivenHelper {

  /** Shortcut for Some type to make case tables more concise */
  def S[T](value: T): Some[T] = Some(value)

  /** Shortcut for None type to make case tables more concise */
  def N: None.type = None

  /** Shortcut for Seq type to make case tables more concise */
  def L: Seq.type = Seq
}
