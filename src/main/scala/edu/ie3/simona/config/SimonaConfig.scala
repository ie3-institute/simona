/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import SimonaConfig._

import scala.concurrent.duration.{Duration, DurationInt}
import pureconfig._
import pureconfig.generic.auto._

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

case class SimonaConfig(
    simulationName: String,
    time: TimeConfig,
    input: InputConfig,
    output: OutputConfig,
    runtime: RuntimeConfig,
    powerflow: PowerFlowConfig,
    gridConfig: GridConfig,
    event: EventConfig
)

object SimonaConfig {
  def apply(filePath: Path): SimonaConfig = {
    Try(ConfigSource.file(filePath).loadOrThrow[SimonaConfig]) match {
      case Success(config) => config
      case Failure(exception) =>
        println(Left(s"Error reading configuration: ${exception.getMessage}"));
        throw exception
    }
  }

  case class TimeConfig(
      startDateTime: String,
      endDateTime: String,
      stopOnFailedPowerFlow: Boolean = false,
      schedulerReadyCheckWindow: Option[Int] = None
  )

  final case class PowerFlowConfig(
      maxSweepPowerDeviation: Double,
      sweepTimeOut: Duration = 30.second,
      newtonraphson: NewtonRaphsonConfig,
      resolution: Duration = 1.hour
  )

  final case class NewtonRaphsonConfig(
      epsilon: Seq[Double],
      iterations: Int
  )

  final case class GridConfig(
      refSystems: Seq[RefSystemConfig]
  )

  final case class RefSystemConfig(
      sNom: String,
      vNom: String,
      voltLvls: Option[Seq[VoltLvlConfig]],
      gridIds: Option[Seq[String]]
  )

  final case class VoltLvlConfig(
      id: String,
      vNom: String
  )

  final case class EventConfig(
      listener: Option[Seq[EventListenerConfig]]
  )

  final case class EventListenerConfig(
      fullClassPath: String,
      eventsToProcess: Seq[String]
  )
}
