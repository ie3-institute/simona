/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.integration.common

trait IntegrationSpecCommon {

  /* ATTENTION: Do not change this file to a path within "input". If you come to this point because the CI
   * or some of your tests are failing you very likely have altered the vn_simona.conf. This config although
   * is NOT meant to be altered. Instead you should always use a delta config and only override the values and
   * files of vn_simona/vn_simona.conf. Delta configs can be created by including the config you want to change
   * parameters from via include <path-to-config> (e.g. include "input/samples/vn_simona/vn_simona.conf") at the
   * beginning of your config file and then just override the parameters you want to change! */
  val configFile: String = "input/samples/vn_simona/vn_simona.conf"

}
