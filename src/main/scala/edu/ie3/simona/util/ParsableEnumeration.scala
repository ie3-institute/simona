/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.util.StringUtils

/** Abstract class, that offers enhanced functionality to parse an enum value
  * from a given String input.
  */
abstract class ParsableEnumeration extends Enumeration {

  /** Get a matching enum value from string input. If no matching behaviour can
    * be found, a NoSuchElementException is thrown.
    *
    * @param input
    *   Input string
    * @return
    *   Matching load model behaviour
    */
  def apply(input: String): Value = {
    val cleanedInput = prepareInputString(input)
    withName(cleanedInput)
  }

  /** Checks, if the given input is an eligible description of a enum value
    *
    * @param input
    *   Input string
    * @return
    *   true, if it is eligible, false if not
    */
  def isEligibleInput(input: String): Boolean = {
    val cleanedInput = prepareInputString(input)
    values.exists(_.toString == cleanedInput)
  }

  /** Uniform processing of Strings: Replaces all non word characters, trims it
    * and converts it to lower case
    *
    * @param input
    *   String to prepare
    * @return
    *   Processed String
    */
  private def prepareInputString(input: String): String =
    StringUtils.cleanString(input.trim).toLowerCase
}
