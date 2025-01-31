package edu.ie3.simona.config

import edu.ie3.simona.config.ControlConfig.TransformerControlGroup

import java.util.UUID

final case class ControlConfig (
    transformer: List[TransformerControlGroup],
                               )

object ControlConfig {

  final case class TransformerControlGroup(
                                            measurements: List[UUID],
                                            transformers: List[UUID],
                                            vMax: Double,
                                            vMin: Double,
                                          )

}
