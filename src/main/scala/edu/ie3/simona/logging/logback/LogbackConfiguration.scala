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

import java.nio.file.Path

object LogbackConfiguration extends LazyLogging {

  def default(logLevel: String, console: Option[String] = None)(
      logPath: Path
  ): Unit = {
    // if no config log level is given, we use the log level of the file
    val consoleLogLevel = console.getOrElse(logLevel)

    LoggerFactory.getILoggerFactory match {
      case loggerContext: LoggerContext =>
        val rootLogger = loggerContext.getLogger("root")

        // Since logback configuration is static
        val logFile = logPath.resolve("simona.log")

        rootLogger.addAppender(
          createFileAppender(
            logFile,
            logLevel,
            "simona-default",
            loggerContext,
          )
        )

        rootLogger.addAppender(
          createConsoleAppender(consoleLogLevel, loggerContext)
        )

      case factory =>
        logger.error(
          s"Cannot configure simulation run logger! Invalid factory: $factory"
        )
    }
  }

  /** Creates a FileAppender that logs to given path.
    */
  private def createFileAppender(
      logPath: Path,
      logLevel: String,
      appenderName: String,
      loggerContext: LoggerContext,
  ): FileAppender[ILoggingEvent] = {
    val (layoutEncoder, filter) = configure(logLevel, loggerContext)

    val fileAppender = new FileAppender[ILoggingEvent]
    fileAppender.setFile(logPath.toString)
    fileAppender.setEncoder(layoutEncoder)
    fileAppender.setContext(loggerContext)
    fileAppender.setName(appenderName)
    fileAppender.addFilter(filter)
    fileAppender.start()

    fileAppender
  }

  /** Creates a ConsoleAppender that logs to the console.
    */
  private def createConsoleAppender(
      logLevel: String,
      loggerContext: LoggerContext,
  ): ConsoleAppender[ILoggingEvent] = {
    val (layoutEncoder, filter) = configure(logLevel, loggerContext)

    val consoleAppender = new ConsoleAppender[ILoggingEvent]
    consoleAppender.setEncoder(layoutEncoder)
    consoleAppender.setContext(loggerContext)
    consoleAppender.setName("STDOUT")
    consoleAppender.addFilter(filter)
    consoleAppender.start()
    consoleAppender
  }

  /** Basic configuration for an appender.
    * @param logLevel
    *   The log level to use.
    * @param loggerContext
    *   The context of the logger.
    */
  private def configure(
      logLevel: String,
      loggerContext: LoggerContext,
  ): (PatternLayoutEncoder, ThresholdFilter) = {
    val layoutEncoder = new PatternLayoutEncoder
    layoutEncoder.setPattern("%d{HH:mm:ss.SSS} %-5level %logger{36} - %msg%n")
    layoutEncoder.setContext(loggerContext)
    layoutEncoder.start()

    val filter = new ThresholdFilter()
    filter.setLevel(logLevel)
    filter.start()

    (layoutEncoder, filter)
  }

}
