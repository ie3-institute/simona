/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.datamodel.models.input.AssetInput

/** Providing extended functionality to input data.
  */
object InputUtils {

  /** Providing extended functionality to an [[AssetInput]].
    */
  extension (asset: AssetInput) {

    /** Returning an identifying string for an asset, including asset type, id
      * and uuid.
      *
      * @return
      *   The identifying string.
      */
    def identifier: String =
      s"${asset.getClass.getSimpleName}[${asset.getId}/${asset.getUuid}]"
  }

}
