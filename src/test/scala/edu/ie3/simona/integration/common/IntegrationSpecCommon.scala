/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.integration.common

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.{
  FixedFeedInResult,
  LoadResult,
  PvResult,
  WecResult
}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.util.ResultFileHierarchy
import org.scalatest.matchers.should
import org.scalatest.{Assertion, OptionValues, PrivateMethodTester}

import java.io.File
import java.util
import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters._

trait IntegrationSpecCommon
    extends should.Matchers
    with OptionValues
    with PrivateMethodTester {

  /* ATTENTION: Do not change this file to a path within "input". If you come to this point because the CI
   * or some of your tests are failing you very likely have altered the vn_simona.conf. This config although
   * is NOT meant to be altered. Instead you should always use a delta config and only override the values and
   * files of vn_simona/vn_simona.conf. Delta configs can be created by including the config you want to change
   * parameters from via include <path-to-config> (e.g. include "input/vn_simona/vn_simona.conf") at the
   * beginning of your config file and then just override the parameters you want to change! */
  val configFile: String = "input/samples/vn_simona/vn_simona.conf"

  protected def checkResults(
      resultFileHierarchy: ResultFileHierarchy,
      runtimeEventQueue: util.Queue[RuntimeEvent]
  ): Assertion = {
    /* check the results */
    // check configs
    val configOutputDir = new File(resultFileHierarchy.configOutputDir)

    configOutputDir.isDirectory shouldBe true
    configOutputDir.listFiles.toVector.size shouldBe 1

    // check runtime event queue for the expected runtime events
    checkRuntimeEvents(runtimeEventQueue.asScala)

    val fixedFeedInResultFileContent = getFileSource(
      resultFileHierarchy,
      classOf[FixedFeedInResult]
    ).getLines().toVector
    fixedFeedInResultFileContent.size shouldBe 2
    fixedFeedInResultFileContent.headOption.value shouldBe "uuid,input_model,p,q,time"

    val loadResultFileContent = getFileSource(
      resultFileHierarchy,
      classOf[LoadResult]
    ).getLines().toVector
    loadResultFileContent.size shouldBe 497
    loadResultFileContent.headOption.value shouldBe "uuid,input_model,p,q,time"

    // check result data
    val pvResultFileContent = getFileSource(
      resultFileHierarchy,
      classOf[PvResult]
    ).getLines().toVector
    pvResultFileContent.size shouldBe 190
    pvResultFileContent.headOption.value shouldBe "uuid,input_model,p,q,time"

    val wecResultFileContent = getFileSource(
      resultFileHierarchy,
      classOf[WecResult]
    ).getLines().toVector
    wecResultFileContent.size shouldBe 7
    wecResultFileContent.headOption.value shouldBe "uuid,input_model,p,q,time"
  }

  private def getFileSource(
      resultFileHierarchy: ResultFileHierarchy,
      entityClass: Class[_ <: ResultEntity]
  ): BufferedSource = {
    Source.fromFile(
      resultFileHierarchy.rawOutputDataFilePaths.getOrElse(
        entityClass,
        fail(s"Unable to get output path for result entity: $entityClass")
      )
    )
  }

  private def checkRuntimeEvents(
      runtimeEvents: Iterable[RuntimeEvent]
  ): Unit = {
    runtimeEvents.toVector.size shouldBe 12
    val groupedRuntimeEvents = runtimeEvents.toVector.groupBy {
      case Initializing            => Initializing
      case InitComplete(_)         => InitComplete
      case Simulating(_, _)        => Simulating
      case CheckWindowPassed(_, _) => CheckWindowPassed
      case Done(_, _, _, _)        => Done
      case other                   => fail(s"Unexpected runtime event: $other")
    }

    groupedRuntimeEvents.size shouldBe 5
    groupedRuntimeEvents.keySet should contain allOf (Simulating, CheckWindowPassed, InitComplete, Initializing, Done)

    groupedRuntimeEvents
      .get(Simulating)
      .foreach(simulatingEvents => {
        simulatingEvents.size shouldBe 1
        simulatingEvents.headOption.foreach(_ shouldBe Simulating(0L, 7200L))
      })

    groupedRuntimeEvents
      .get(CheckWindowPassed)
      .foreach(checkWindowsPassed => {
        checkWindowsPassed.size shouldBe 8
        checkWindowsPassed.foreach {
          case CheckWindowPassed(tick, _) =>
            tick % 900L shouldBe 0L // config has 900 sec as check window value
          case invalidEvent =>
            fail(
              s"Invalid event when expecting CheckWindowPassed: $invalidEvent"
            )
        }
      })

    groupedRuntimeEvents
      .get(InitComplete)
      .foreach(initCompletes => {
        initCompletes.size shouldBe 1
      })

    groupedRuntimeEvents
      .get(Initializing)
      .foreach(initializings => {
        initializings.size shouldBe 1
      })

    groupedRuntimeEvents
      .get(Done)
      .foreach(dones => {
        dones.size shouldBe 1
        dones.headOption.foreach {
          case Done(tick, _, noOfFailedPF, errorInSim) =>
            tick shouldBe 7200L
            errorInSim shouldBe false
            noOfFailedPF shouldBe 0
          case invalidEvent =>
            fail(s"Invalid event when expecting Done: $invalidEvent")
        }
      })
  }
}
