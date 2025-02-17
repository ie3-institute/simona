/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import com.typesafe.config.ConfigValueFactory
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult,
}
import edu.ie3.datamodel.models.result.system.PvResult
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.ResultEvent.{
  ParticipantResultEvent,
  PowerFlowResultEvent,
}
import edu.ie3.simona.io.result.ResultSinkType.Csv
import edu.ie3.simona.io.result.{ResultEntitySink, ResultSinkType}
import edu.ie3.simona.test.common.result.PowerFlowResultData
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import edu.ie3.util.TimeUtil
import edu.ie3.util.io.FileIOUtils
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ActorTestKit,
  ScalaTestWithActorTestKit,
}
import org.apache.pekko.testkit.TestKit.awaitCond

import java.io.{File, FileInputStream}
import java.util.UUID
import java.util.zip.GZIPInputStream
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.language.postfixOps

class ResultEventListenerSpec
    extends ScalaTestWithActorTestKit(
      ActorTestKit.ApplicationTestConfig.withValue(
        "org.apache.pekko.actor.testkit.typed.filter-leeway",
        ConfigValueFactory.fromAnyRef("10s"),
      )
    )
    with UnitSpec
    with IOTestCommons
    with PowerFlowResultData
    with ThreeWindingResultTestData
    with Transformer3wResultSupport {
  val simulationName = "testSim"
  val resultEntitiesToBeWritten: Set[Class[_ <: ResultEntity]] = Set(
    classOf[PvResult],
    classOf[NodeResult],
    classOf[Transformer2WResult],
    classOf[Transformer3WResult],
    classOf[SwitchResult],
    classOf[LineResult],
  )

  private val timeoutDuration: Duration = 30.seconds

  // the OutputFileHierarchy
  private def resultFileHierarchy(
      runId: Int,
      fileFormat: String,
      classes: Set[Class[_ <: ResultEntity]] = resultEntitiesToBeWritten,
      compressResults: Boolean = false,
  ): ResultFileHierarchy = {
    val resultSinkType: ResultSinkType =
      Csv(fileFormat, "", "", compressResults)

    ResultFileHierarchy(
      outputDir = testTmpDir + File.separator + runId,
      simulationName,
      ResultEntityPathConfig(
        classes,
        resultSinkType,
      ),
    )
  }

  def createDir(
      resultFileHierarchy: ResultFileHierarchy
  ): Iterable[Future[ResultEntitySink]] = {
    val initializeSinks: PrivateMethod[Iterable[Future[ResultEntitySink]]] =
      PrivateMethod[Iterable[Future[ResultEntitySink]]](
        Symbol("initializeSinks")
      )

    ResultEventListener invokePrivate initializeSinks(resultFileHierarchy)
  }

  private def getFileLinesLength(file: File) = {
    val fileSource = Source.fromFile(file)
    val length = fileSource.getLines().size
    fileSource.close()
    length
  }

  override protected def afterAll(): Unit = {
    // cleanup
    FileIOUtils.deleteRecursively(testTmpDir)
    super.afterAll()
  }

  "A ResultEventListener" when {
    "setting everything up" should {
      "initialize its sinks correctly" in {
        val fileHierarchy = resultFileHierarchy(1, ".csv")
        Await.ready(
          Future.sequence(createDir(fileHierarchy)),
          60 seconds,
        )

        // after the creation of the listener, it is expected that a corresponding raw result data file is present
        val outputFile = fileHierarchy.rawOutputDataFilePaths
          .getOrElse(
            classOf[PvResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
            ),
          )
          .toFile

        assert(outputFile.exists)
        assert(outputFile.isFile)
      }

      "check if actor dies when it should die" in {
        val fileHierarchy =
          resultFileHierarchy(2, ".ttt", Set(classOf[Transformer3WResult]))
        val deathWatch = createTestProbe("deathWatch")
        val listener = spawn(
          ResultEventListener(
            fileHierarchy
          )
        )

        listener ! DelayedStopHelper.FlushAndStop
        deathWatch expectTerminated (listener, 10 seconds)
      }
    }

    "handling ordinary results" should {
      "process a valid participants result correctly" in {
        val specificOutputFileHierarchy = resultFileHierarchy(3, ".csv")

        val listenerRef = spawn(
          ResultEventListener(
            specificOutputFileHierarchy
          )
        )

        listenerRef ! ParticipantResultEvent(dummyPvResult)

        val outputFile = specificOutputFileHierarchy.rawOutputDataFilePaths
          .getOrElse(
            classOf[PvResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
            ),
          )
          .toFile

        // wait until output file exists (headers are flushed out immediately):
        awaitCond(
          outputFile.exists(),
          interval = 500.millis,
          max = timeoutDuration,
        )

        // stop listener so that result is flushed out
        listenerRef ! DelayedStopHelper.FlushAndStop

        // wait until all lines have been written out:
        awaitCond(
          getFileLinesLength(outputFile) == 2,
          interval = 500.millis,
          max = timeoutDuration,
        )

        val resultFileSource = Source.fromFile(outputFile)

        val resultFileLines = resultFileSource.getLines().toSeq

        resultFileLines.size shouldBe 2
        resultFileLines.lastOption.getOrElse(
          fail(
            "Cannot get csv row that should have been written out by the listener!"
          )
        ) shouldBe dummyPvResultDataString

        resultFileSource.close()
      }

      "process a valid power flow result correctly" in {
        val specificOutputFileHierarchy = resultFileHierarchy(4, ".csv")
        val listenerRef = spawn(
          ResultEventListener(
            specificOutputFileHierarchy
          )
        )

        listenerRef ! PowerFlowResultEvent(
          Iterable(dummyNodeResult),
          Iterable(dummySwitchResult),
          Iterable(dummyLineResult),
          Iterable(dummyTrafo2wResult),
          Iterable.empty[PartialTransformer3wResult],
        )

        val outputFiles = Map(
          dummyNodeResultString -> specificOutputFileHierarchy.rawOutputDataFilePaths
            .getOrElse(
              classOf[NodeResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[NodeResult].getSimpleName}' from outputFileHierarchy!'"
              ),
            ),
          dummySwitchResultString ->
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[SwitchResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[SwitchResult].getSimpleName}' from outputFileHierarchy!'"
              ),
            ),
          dummyLineResultDataString -> specificOutputFileHierarchy.rawOutputDataFilePaths
            .getOrElse(
              classOf[LineResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[LineResult].getSimpleName}' from outputFileHierarchy!'"
              ),
            ),
          dummyTrafo2wResultDataString -> specificOutputFileHierarchy.rawOutputDataFilePaths
            .getOrElse(
              classOf[Transformer2WResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[Transformer2WResult].getSimpleName}' from outputFileHierarchy!'"
              ),
            ),
        ).map { case (dummyString, path) =>
          (dummyString, path.toFile)
        }

        // wait until all output files exist (headers are flushed out immediately):
        awaitCond(
          outputFiles.values.map(_.exists()).forall(identity),
          interval = 500.millis,
          max = timeoutDuration,
        )

        // stop listener so that result is flushed out
        listenerRef ! DelayedStopHelper.FlushAndStop

        // wait until all lines have been written out:
        awaitCond(
          !outputFiles.values.exists(file => getFileLinesLength(file) < 2),
          interval = 500.millis,
          max = timeoutDuration,
        )

        outputFiles.foreach { case (resultRowString, outputFile) =>
          val resultFileSource = Source.fromFile(outputFile)

          val resultFileLines = resultFileSource.getLines().toSeq

          resultFileLines.size shouldBe 2
          resultFileLines.lastOption.getOrElse(
            fail(
              "Cannot get csv row that should have been written out by the listener!"
            )
          ) shouldBe resultRowString

          resultFileSource.close()
        }
      }
    }

    "handling three winding transformer results" should {
      def powerflow3wResult(
          partialResult: PartialTransformer3wResult
      ): PowerFlowResultEvent =
        PowerFlowResultEvent(
          Iterable.empty[NodeResult],
          Iterable.empty[SwitchResult],
          Iterable.empty[LineResult],
          Iterable.empty[Transformer2WResult],
          Iterable(partialResult),
        )

      "correctly reacts on received results" in {
        val fileHierarchy =
          resultFileHierarchy(5, ".csv", Set(classOf[Transformer3WResult]))
        val listener = spawn(
          ResultEventListener(
            fileHierarchy
          )
        )

        val outputFile = fileHierarchy.rawOutputDataFilePaths
          .getOrElse(
            classOf[Transformer3WResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[Transformer3WResult].getSimpleName}' from outputFileHierarchy!'"
            ),
          )
          .toFile

        /* The result file is created at start up and only contains a headline. */
        awaitCond(
          outputFile.exists(),
          interval = 500.millis,
          max = timeoutDuration,
        )
        getFileLinesLength(outputFile) shouldBe 1

        /* Face the listener with data, as long as they are not comprehensive */
        listener ! powerflow3wResult(resultA)

        listener ! powerflow3wResult(resultC)

        /* Also add unrelated result for different input model */
        val otherResultA = resultA.copy(input = UUID.randomUUID())
        listener ! powerflow3wResult(otherResultA)

        /* Add result A again, which should lead to a failure internally,
        but everything should still continue normally
         */
        listener ! powerflow3wResult(resultA)

        /* Make sure, that there still is no content in file */
        getFileLinesLength(outputFile) shouldBe 1

        /* Complete awaited result */
        listener ! powerflow3wResult(resultB)

        // stop listener so that result is flushed out
        listener ! DelayedStopHelper.FlushAndStop

        /* Await that the result is written */
        awaitCond(
          getFileLinesLength(outputFile) == 2,
          interval = 500.millis,
          max = timeoutDuration,
        )
        /* Check the result */
        val resultFileSource = Source.fromFile(outputFile)
        val resultFileLines = resultFileSource.getLines().toSeq

        resultFileLines.size shouldBe 2
        val resultLine = resultFileLines.lastOption.getOrElse(
          fail(
            "Cannot get csv row that should have been written out by the listener!"
          )
        )

        resultLine shouldBe "2.0,1.0,4.0,3.0,6.0,5.0,40d02538-d8dd-421c-8e68-400f1da170c7,-5," + TimeUtil.withDefaults
          .toString(time)

        resultFileSource.close()
      }
    }

    "shutting down" should {
      "shutdown and compress the data when requested to do so without any errors" in {
        val specificOutputFileHierarchy =
          resultFileHierarchy(6, ".csv.gz", compressResults = true)
        val listenerRef = spawn(
          ResultEventListener(
            specificOutputFileHierarchy
          )
        )
        ResultSinkType.Csv(fileFormat = ".csv.gz")

        listenerRef ! ParticipantResultEvent(dummyPvResult)

        val outputFile = new File(
          ".gz$".r.replaceAllIn(
            specificOutputFileHierarchy.rawOutputDataFilePaths
              .getOrElse(
                classOf[PvResult],
                fail(
                  s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
                ),
              )
              .toString,
            "",
          )
        )

        awaitCond(
          outputFile.exists(),
          interval = 500.millis,
          max = timeoutDuration,
        )

        assert(outputFile.exists(), "Output file does not exist")

        // stopping the actor should wait until existing messages within an actor are fully processed
        // otherwise it might happen, that the shutdown is triggered even before the just send ParticipantResultEvent
        // reached the listener
        // this also triggers the compression of result files
        listenerRef ! DelayedStopHelper.FlushAndStop

        // shutdown the actor system
        system.terminate()

        // wait until file exists
        awaitCond(
          specificOutputFileHierarchy.rawOutputDataFilePaths
            .getOrElse(
              classOf[PvResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
              ),
            )
            .toFile
            .exists,
          timeoutDuration,
        )

        val compressedFile = specificOutputFileHierarchy.rawOutputDataFilePaths
          .getOrElse(
            classOf[PvResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
            ),
          )
          .toFile
        assert(compressedFile.exists(), "Compressed file does not exist")

        val resultFileSource = Source.fromInputStream(
          new GZIPInputStream(
            new FileInputStream(compressedFile)
          )
        )

        val resultFileLines = resultFileSource.getLines().toSeq
        resultFileLines.size shouldBe 2
        resultFileLines.lastOption.getOrElse(
          fail(
            "Cannot get line that should have been written out by the listener!"
          )
        ) shouldBe "e5ac84d3-c7a5-4870-a42d-837920aec9bb,0.01,0.01,2020-01-30T17:26:44Z"

        resultFileSource.close()
      }
    }
  }
}
