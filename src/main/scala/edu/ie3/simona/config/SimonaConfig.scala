package edu.ie3.simona.config

import SimonaConfig._

import scala.concurrent.duration.{Duration, DurationInt}

case class SimonaConfig(
    time: TimeConfig,
    input: InputConfig,
    output: OutputConfig,
    runtime: RuntimeConfig,
    powerflow: PowerFlowConfig,
    gridConfig: GridConfig,
    event: Option[EventConfig]
)

object SimonaConfig {

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
      listener: Seq[EventListenerConfig]
  )

  final case class EventListenerConfig(
      fullClassPath: String,
      eventsToProcess: Seq[String]
  )
}

