/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.{ActorRef, Props}
import breeze.math.Complex
import edu.ie3.simona.agent.grid.SweepValueStore
import edu.ie3.simona.event.Event
import edu.ie3.simona.event.listener.DBFSListener.DBFSResults
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.sim.SimonaSim.ServiceInitComplete
import edu.ie3.simona.util.ResultFileHierarchy

import java.io.{File, PrintWriter}
import java.util.UUID

object DBFSListener {

  final case class DBFSResults(
      senderName: String,
      tick: Long,
      sweepData: Map[Int, SweepValueStore]
  ) extends Event

  def props(
      resultFileHierarchy: ResultFileHierarchy,
      supervisor: ActorRef
  ): Props =
    Props(
      new DBFSListener(
        resultFileHierarchy,
        supervisor
      )
    )
}

final class DBFSListener(
    resultFileHierarchy: ResultFileHierarchy,
    supervisor: ActorRef
) extends SimonaListener
    with SimonaActorLogging {

  // unblock supervisor to go on with simulation
  supervisor ! ServiceInitComplete

  private val outputWriter =
    scala.collection.mutable.Map.empty[String, PrintWriter]
  private val outputHeadline =
    scala.collection.mutable.Map.empty[String, Vector[UUID]]
  private val sep = ";"

  override def receive: Receive = {
    case DBFSResults(senderName, tick, sweepData) =>
      handleSweepData(senderName, tick, sweepData)
    case _ => // drop other messages
  }

  private def handleSweepData(
      senderName: String,
      tick: Long,
      sweepData: Map[Int, SweepValueStore]
  ): Unit = {

    val writer = outputWriter.getOrElse(
      senderName, {
        // create writer
        val outputFile =
          s"${resultFileHierarchy.rawOutputDataDir + File.separator}dbfs_v_mag_pu_$senderName.csv"
        val writer = new PrintWriter(outputFile)
        // create and write headline
        sweepData.headOption
          .map(sweepData => createHeadline(senderName, sweepData._2))
          .foreach(writer.write)
        writer.flush()
        // add data to outputWriterMap
        this.outputWriter += (senderName -> writer)
        writer
      }
    )

    // write row data
    sweepData.foreach { case (sweepNo, sweepValueStore: SweepValueStore) =>
      createEntryRow(senderName, sweepNo, tick, sweepValueStore).foreach(
        writer.write
      )
      writer.flush()
    }
  }

  def createHeadline(
      senderName: String,
      sweepValueStore: SweepValueStore
  ): String = {
    // tick, sweepNo, local_pf_counter, {shortNodeUuids}
    val uuids = sweepValueStore.sweepData
      .map(_.nodeUuid.toString.substring(0, 8))
      .mkString(sep)
    // we want to keep this order for later
    this.outputHeadline += (senderName -> sweepValueStore.sweepData.map(
      _.nodeUuid
    ))
    s"tick${sep}sweep_no${sep}local_pf_counter${sep}$uuids\n"
  }

  def createEntryRow(
      senderName: String,
      sweepNo: Int,
      tick: Long,
      sweepValueStore: SweepValueStore
  ): Option[String] = {
    // tick, sweepNo, local_pf_counter, {results}
    val sweepVoltages = sweepValueStore.sweepData
      .map(sweepValueStoreData =>
        sweepValueStoreData.nodeUuid -> sweepValueStoreData.stateData.voltage
      )
      .toMap
    outputHeadline
      .get(senderName)
      .map(nodeUuids => {
        nodeUuids.flatMap(sweepVoltages.get(_).map(calcVMag)).mkString(sep)
      })
      .map(data =>
        s"$tick${sep}$sweepNo${sep}${sweepValueStore.localIterationCounter}${sep}$data\n"
      )
  }

  private def calcVMag(voltage: Complex): Double =
    voltage.abs

  override def postStop(): Unit = {
    outputWriter.values.foreach(writer => {
      writer.flush()
      writer.close()
    })
  }
}
