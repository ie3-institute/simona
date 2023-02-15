/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import akka.actor.testkit.typed.scaladsl.{
  ActorTestKit,
  ScalaTestWithActorTestKit
}
import akka.stream.Materializer
import akka.testkit.TestKit.awaitCond
import com.typesafe.config.ConfigValueFactory
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult
}
import edu.ie3.datamodel.models.result.system.PvResult
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.ResultEvent.{
  ParticipantResultEvent,
  PowerFlowResultEvent
}
import edu.ie3.simona.io.result.{ResultEntitySink, ResultSinkType}
import edu.ie3.simona.ontology.messages.StopMessage
import edu.ie3.simona.test.common.result.PowerFlowResultData
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import edu.ie3.util.io.FileIOUtils

import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, _}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.language.postfixOps

class ResultEventListenerSpec
    extends ScalaTestWithActorTestKit(
      ActorTestKit.ApplicationTestConfig.withValue(
        "akka.actor.testkit.typed.filter-leeway",
        ConfigValueFactory.fromAnyRef("10s")
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
    classOf[LineResult]
  )

  private val timeoutDuration: Duration = 30.seconds

  // the OutputFileHierarchy
  private def resultFileHierarchy(
      runId: Int,
      fileFormat: String,
      classes: Set[Class[_ <: ResultEntity]] = resultEntitiesToBeWritten
  ): ResultFileHierarchy =
    ResultFileHierarchy(
      outputDir = testTmpDir + File.separator + runId,
      simulationName,
      ResultEntityPathConfig(
        classes,
        ResultSinkType.Csv(fileFormat = fileFormat)
      ),
      createDirs = true
    )

  def createDir(
      resultFileHierarchy: ResultFileHierarchy
  ): Iterable[Future[ResultEntitySink]] = {
    val materializer: Materializer = Materializer(system)

    val initializeSinks: PrivateMethod[Iterable[Future[ResultEntitySink]]] =
      PrivateMethod[Iterable[Future[ResultEntitySink]]](
        Symbol("initializeSinks")
      )

    ResultEventListener invokePrivate initializeSinks(
      resultFileHierarchy,
      materializer
    )
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
          60 seconds
        )

        // after the creation of the listener, it is expected that a corresponding raw result data file is present
        val outputFile = new File(
          fileHierarchy.rawOutputDataFilePaths.getOrElse(
            classOf[PvResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
            )
          )
        )

        assert(outputFile.exists)
        assert(outputFile.isFile)
      }

      "check if actor dies when it should die" in {
        val fileHierarchy =
          resultFileHierarchy(2, ".ttt", Set(classOf[Transformer3WResult]))
        val testProbe = testKit.createTestProbe("testProbe")
        val listener = testKit.spawn(
          ResultEventListener(
            fileHierarchy
          )
        )

        listener.tell(StopMessage(true))
        testProbe expectTerminated (listener, 10 seconds)
      }
    }

    "handling ordinary results" should {
      "process a valid participants result correctly" in {
        val specificOutputFileHierarchy = resultFileHierarchy(3, ".csv")

        val listenerRef = testKit.spawn(
          ResultEventListener(
            specificOutputFileHierarchy
          )
        )

        listenerRef ! ParticipantResultEvent(dummyPvResult)

        val outputFile = new File(
          specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
            classOf[PvResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
            )
          )
        )

        // wait until output file exists (headers are flushed out immediately):
        awaitCond(
          outputFile.exists(),
          interval = 500.millis,
          max = timeoutDuration
        )

        // stop listener so that result is flushed out
        listenerRef ! StopMessage(true)

        // wait until all lines have been written out:
        awaitCond(
          getFileLinesLength(outputFile) == 2,
          interval = 500.millis,
          max = timeoutDuration
        )

        val resultFileSource = Source.fromFile(outputFile)

        val resultFileLines = resultFileSource.getLines().toVector

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
        val listenerRef = testKit.spawn(
          ResultEventListener(
            specificOutputFileHierarchy
          )
        )

        listenerRef ! PowerFlowResultEvent(
          Vector(dummyNodeResult),
          Vector(dummySwitchResult),
          Vector(dummyLineResult),
          Vector(dummyTrafo2wResult),
          Vector.empty[PartialTransformer3wResult]
        )

        val outputFiles = Map(
          dummyNodeResultString -> new File(
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[NodeResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[NodeResult].getSimpleName}' from outputFileHierarchy!'"
              )
            )
          ),
          dummySwitchResultString -> new File(
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[SwitchResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[SwitchResult].getSimpleName}' from outputFileHierarchy!'"
              )
            )
          ),
          dummyLineResultDataString -> new File(
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[LineResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[LineResult].getSimpleName}' from outputFileHierarchy!'"
              )
            )
          ),
          dummyTrafo2wResultDataString -> new File(
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[Transformer2WResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[Transformer2WResult].getSimpleName}' from outputFileHierarchy!'"
              )
            )
          )
        )

        // wait until all output files exist (headers are flushed out immediately):
        awaitCond(
          outputFiles.values.map(_.exists()).forall(identity),
          interval = 500.millis,
          max = timeoutDuration
        )

        // stop listener so that result is flushed out
        listenerRef ! StopMessage(true)

        // wait until all lines have been written out:
        awaitCond(
          !outputFiles.values.exists(file => getFileLinesLength(file) < 2),
          interval = 500.millis,
          max = timeoutDuration
        )

        outputFiles.foreach { case (resultRowString, outputFile) =>
          val resultFileSource = Source.fromFile(outputFile)

          val resultFileLines = resultFileSource.getLines().toVector

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
      val registerPartialTransformer3wResult =
        PrivateMethod[Map[Transformer3wKey, AggregatedTransformer3wResult]](
          Symbol("registerPartialTransformer3wResult")
        )
      val fileHierarchy =
        resultFileHierarchy(5, ".csv", Set(classOf[Transformer3WResult]))
      val listener = testKit.spawn(
        ResultEventListener(
          fileHierarchy
        )
      )

      "register a fresh entry, when nothing yet is apparent" in {
        val actual =
          ResultEventListener invokePrivate registerPartialTransformer3wResult(
            resultA,
            Map.empty[Transformer3wKey, AggregatedTransformer3wResult]
          )
        actual.size shouldBe 1
        actual.get(Transformer3wKey(inputModel, time)) match {
          case Some(AggregatedTransformer3wResult(Some(a), None, None)) =>
            a shouldBe resultA
          case Some(value) => fail(s"Received the wrong value: '$value'")
          case None        => fail("Expected to get a result")
        }
      }

      "neglects, if something yet has been sent" in {
        val results = Map(
          Transformer3wKey(
            resultA.input,
            resultA.time
          ) -> AggregatedTransformer3wResult(
            Some(resultA),
            None,
            None
          )
        )

        val actual =
          ResultEventListener invokePrivate registerPartialTransformer3wResult(
            resultA,
            results
          )
        actual.size shouldBe 1
        actual.get(Transformer3wKey(inputModel, time)) match {
          case Some(AggregatedTransformer3wResult(Some(a), None, None)) =>
            a shouldBe resultA
          case Some(value) => fail(s"Received the wrong value: '$value'")
          case None        => fail("Expected to get a result")
        }
      }

      "appends a new result, if that key yet is apparent" in {
        val results = Map(
          Transformer3wKey(
            resultA.input,
            resultA.time
          ) -> AggregatedTransformer3wResult(
            Some(resultA),
            None,
            None
          )
        )

        val actual =
          ResultEventListener invokePrivate registerPartialTransformer3wResult(
            resultB,
            results
          )
        actual.size shouldBe 1
        actual.get(Transformer3wKey(inputModel, time)) match {
          case Some(AggregatedTransformer3wResult(Some(a), Some(b), None)) =>
            a shouldBe resultA
            b shouldBe resultB
          case Some(value) => fail(s"Received the wrong value: '$value'")
          case None        => fail("Expected to get a result")
        }
      }

      val flushComprehensiveResults =
        PrivateMethod[Map[Transformer3wKey, AggregatedTransformer3wResult]](
          Symbol("flushComprehensiveResults")
        )
      val resultANext = resultA.copy(time = resultA.time.plusHours(1L))

      /** Dummy sink, that only puts the received results to a mutable set
        *
        * @param results
        *   All results, that have been received
        */
      final case class DummySink(
          results: mutable.Set[ResultEntity] = mutable.Set.empty[ResultEntity]
      ) extends ResultEntitySink {
        override def handleResultEntity(resultEntity: ResultEntity): Unit =
          results.add(resultEntity)
        override def close(): Unit = {}
      }
      val sink = DummySink()
      val sinks = Map(
        classOf[Transformer3WResult] -> sink
      )
      "not flush anything, if nothing is ready to be flushed" in {
        val results = Map(
          Transformer3wKey(
            resultA.input,
            resultA.time
          ) -> AggregatedTransformer3wResult(
            Some(resultA),
            None,
            Some(resultC)
          ),
          Transformer3wKey(
            resultANext.input,
            resultANext.time
          ) -> AggregatedTransformer3wResult(
            Some(resultANext),
            None,
            None
          )
        )
        val actual =
          ResultEventListener invokePrivate flushComprehensiveResults(
            results,
            sinks
          )
        actual.size shouldBe 2
        sink.results.isEmpty shouldBe true
      }

      "flush comprehensive results" in {
        val results = Map(
          Transformer3wKey(
            resultA.input,
            resultA.time
          ) -> AggregatedTransformer3wResult(
            Some(resultA),
            Some(resultB),
            Some(resultC)
          ),
          Transformer3wKey(
            resultANext.input,
            resultANext.time
          ) -> AggregatedTransformer3wResult(
            Some(resultANext),
            None,
            None
          )
        )
        val actual =
          ResultEventListener invokePrivate flushComprehensiveResults(
            results,
            sinks
          )
        actual.size shouldBe 1
        sink.results.headOption match {
          case Some(result) => result shouldBe expected
          case None         => fail("Expected to get a result.")
        }
      }

      "correctly reacts on received results" in {
        val outputFile = new File(
          fileHierarchy.rawOutputDataFilePaths.getOrElse(
            classOf[Transformer3WResult],
            fail(
              s"Cannot get filepath for raw result file of class '${classOf[Transformer3WResult].getSimpleName}' from outputFileHierarchy!'"
            )
          )
        )
        /* The result file is created at start up and only contains a head line. */
        awaitCond(
          outputFile.exists(),
          interval = 500.millis,
          max = timeoutDuration
        )
        getFileLinesLength(outputFile) shouldBe 1

        /* Face the listener with data, as long as they are not comprehensive */
        listener ! PowerFlowResultEvent(
          Vector.empty[NodeResult],
          Vector.empty[SwitchResult],
          Vector.empty[LineResult],
          Vector.empty[Transformer2WResult],
          Vector(resultA)
        )
        listener ! PowerFlowResultEvent(
          Vector.empty[NodeResult],
          Vector.empty[SwitchResult],
          Vector.empty[LineResult],
          Vector.empty[Transformer2WResult],
          Vector(resultC)
        )

        /* Make sure, that there still is no content in file */
        getFileLinesLength(outputFile) shouldBe 1

        /* Complete awaited results */
        listener ! PowerFlowResultEvent(
          Vector.empty[NodeResult],
          Vector.empty[SwitchResult],
          Vector.empty[LineResult],
          Vector.empty[Transformer2WResult],
          Vector(resultB)
        )

        // stop listener so that result is flushed out
        listener ! StopMessage(true)

        /* Await that the result is written */
        awaitCond(
          getFileLinesLength(outputFile) == 2,
          interval = 500.millis,
          max = timeoutDuration
        )
        /* Check the result */
        val resultFileSource = Source.fromFile(outputFile)
        val resultFileLines = resultFileSource.getLines().toVector

        resultFileLines.size shouldBe 2
        val resultLine = resultFileLines.lastOption.getOrElse(
          fail(
            "Cannot get csv row that should have been written out by the listener!"
          )
        )

        ("[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12},2.0,1.0,4.0,3.0,6.0,5.0,40d02538-d8dd-421c-8e68-400f1da170c7,-5," + time.toString
          .replaceAll("\\[", "\\\\["))
          .replaceAll("\\.", "\\\\.")
          .r
          .matches(resultLine) shouldBe true

        resultFileSource.close()
      }
    }

    "shutting down" should {
      "shutdown and compress the data when requested to do so without any errors" in {
        val specificOutputFileHierarchy = resultFileHierarchy(6, ".csv.gz")
        val listenerRef = testKit.spawn(
          ResultEventListener(
            specificOutputFileHierarchy
          )
        )
        ResultSinkType.Csv(fileFormat = ".csv.gz")

        listenerRef ! ParticipantResultEvent(dummyPvResult)

        val outputFile = new File(
          ".gz$".r.replaceAllIn(
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[PvResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
              )
            ),
            ""
          )
        )

        awaitCond(
          outputFile.exists(),
          interval = 500.millis,
          max = timeoutDuration
        )

        // graceful shutdown should wait until existing messages within an actor are fully processed
        // otherwise it might happen, that the shutdown is triggered even before the just send ParticipantResultEvent
        // reached the listener
        // this also triggers the compression of result files
        listenerRef ! StopMessage(true)

        // shutdown the actor system
        system.terminate()

        // wait until file exists
        awaitCond(
          new File(
            specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
              classOf[PvResult],
              fail(
                s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
              )
            )
          ).exists,
          timeoutDuration
        )

        val resultFileSource = Source.fromInputStream(
          new GZIPInputStream(
            new FileInputStream(
              specificOutputFileHierarchy.rawOutputDataFilePaths.getOrElse(
                classOf[PvResult],
                fail(
                  s"Cannot get filepath for raw result file of class '${classOf[PvResult].getSimpleName}' from outputFileHierarchy!'"
                )
              )
            )
          )
        )

        val resultFileLines = resultFileSource.getLines().toVector
        resultFileLines.size shouldBe 2
        resultFileLines.lastOption.getOrElse(
          fail(
            "Cannot get line that should have been written out by the listener!"
          )
        ) shouldBe "7f404c4c-fc12-40de-95c9-b5827a40f18b,e5ac84d3-c7a5-4870-a42d-837920aec9bb,0.01,0.01,2020-01-30T17:26:44Z[UTC]"

        resultFileSource.close()
      }
    }
  }
}
