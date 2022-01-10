/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.logging.logback

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.filter.ThresholdFilter
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.{ConsoleAppender, FileAppender}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory

import java.io.File
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IteratorHasAsScala}

sealed trait LogbackConfiguration {
  def configure: Unit
  def cleanup: Unit
}

object LogbackConfiguration extends LazyLogging {

  final case class Default(logPath: String) extends LogbackConfiguration {

    private val specificFileAppenderName = "simona-default"
    private val loggerContext: Option[LoggerContext] =
      LoggerFactory.getILoggerFactory match {
        case loggerContext: LoggerContext =>
          Some(loggerContext)
        case factory =>
          logger.error(
            s"Cannot configure simulation run logger! Invalid factory: $factory"
          )
          None
      }

    override def configure: Unit = loggerContext.foreach(loggerContext => {

      // see http://www.slf4j.org/legacy.html#jul-to-slf4j for performance impact
      logger.warn("Using jul-to-slf4j - this may impact performance!")
      import org.slf4j.bridge.SLF4JBridgeHandler
      SLF4JBridgeHandler.removeHandlersForRootLogger()
      SLF4JBridgeHandler.install()

      logger.info(
        s"Creating simulation file output 'simona.log' in '${new File(logPath).getAbsolutePath}.'"
      )
      val rootLogger = loggerContext.getLogger("root")
      val log = logPath.concat(File.separator).concat("simona.log")
      // stop all appender of all logger
      loggerContext.getLoggerList.asScala.foreach(
        _.iteratorForAppenders().asScala.foreach(_.stop())
      )
      // remove existing appender to avoid duplicated or invalid logging
      rootLogger.detachAppender(specificFileAppenderName)
      rootLogger.addAppender(
        fileAppender(log, specificFileAppenderName, "INFO", loggerContext)
      )
      logger.info(
        "\n   _____ ______  _______  _   _____       ___    ____ \n  / ___//  _/  |/  / __ \\/ | / /   |     |__ \\  / __ \\\n  \\__ \\ / // /|_/ / / / /  |/ / /| |     __/ / / / / /\n ___/ // // /  / / /_/ / /|  / ___ |    / __/_/ /_/ / \n/____/___/_/  /_/\\____/_/ |_/_/  |_|   /____(_)____/  \n                                                      "
      )
      // start all other appender again
      loggerContext.getLoggerList.asScala.foreach(
        _.iteratorForAppenders().asScala.foreach(_.start())
      )
    })

    override def cleanup: Unit =
      loggerContext.foreach(
        _.getLogger("root").detachAppender(specificFileAppenderName)
      )
  }

  def disableConsoleLogging(enabledClasses: Seq[String]): Unit =
    LoggerFactory.getILoggerFactory match {
      case loggerContext: LoggerContext =>
        val rootLogger = loggerContext.getLogger("root")
        val consoleAppender = rootLogger
          .iteratorForAppenders()
          .asScala
          .filter {
            case _: ConsoleAppender[_] =>
              true
            case _ => false
          }
          .toVector
        // disable all console appender
        consoleAppender.foreach(rootLogger.detachAppender)

        // create new logger for each enabled class + attach all console appender to these loggers
        enabledClasses.foreach(enabledClass => {
          val logger = loggerContext.getLogger(enabledClass)
          consoleAppender.foreach(logger.addAppender)
          logger.info(s"Console logging for class '$enabledClass' enabled!")
          logger.iteratorForAppenders().asScala.foreach(_.start())
        })

      case factory =>
        logger.error(
          s"Cannot configure simulation run logger! Invalid factory: $factory"
        )
    }

  private def fileAppender(
      logPath: String,
      appenderName: String,
      thresholdFilterLevel: String,
      loggerContext: LoggerContext
  ): FileAppender[ILoggingEvent] = {

    val layoutEncoder = new PatternLayoutEncoder
    layoutEncoder.setPattern("%d{HH:mm:ss.SSS} %-5level %logger{36} - %msg%n")
    layoutEncoder.setContext(loggerContext)
    layoutEncoder.start()

    val fileAppender = new FileAppender[ILoggingEvent]
    fileAppender.setFile(logPath)
    fileAppender.setEncoder(layoutEncoder)
    fileAppender.setContext(loggerContext)
    fileAppender.setName(appenderName)
    val filter = new ThresholdFilter()
    filter.setLevel(thresholdFilterLevel)
    filter.start()
    fileAppender.addFilter(filter)
    fileAppender.start()

    fileAppender
  }

}
