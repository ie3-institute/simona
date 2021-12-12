/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.logging.logback

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.filter.ThresholdFilter
import ch.qos.logback.classic.spi.{ILoggingEvent, LoggingEvent}
import ch.qos.logback.core.FileAppender
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory

import java.io.File
import scala.jdk.CollectionConverters.IteratorHasAsScala

object LogbackConfiguration extends LazyLogging {

  def default(logPath: String): Unit = {
    LoggerFactory.getILoggerFactory match {
      case loggerContext: LoggerContext =>
        val rootLogger = loggerContext.getLogger("root")
        val log = logPath.concat(File.separator).concat("simona.log")
        // stop all appenders
        rootLogger.iteratorForAppenders().asScala.foreach(_.stop())
        rootLogger.addAppender(
          fileAppender(log, "simona-default", "INFO", loggerContext)
        )
        logger.info(
          "\n   _____ ______  _______  _   _____       ___    ____ \n  / ___//  _/  |/  / __ \\/ | / /   |     |__ \\  / __ \\\n  \\__ \\ / // /|_/ / / / /  |/ / /| |     __/ / / / / /\n ___/ // // /  / / /_/ / /|  / ___ |    / __/_/ /_/ / \n/____/___/_/  /_/\\____/_/ |_/_/  |_|   /____(_)____/  \n                                                      "
        ) // start all other appenders again
        rootLogger.iteratorForAppenders().asScala.foreach(_.start())
      case factory =>
        logger.error(
          s"Cannot configure simulation run logger! Invalid factory: $factory"
        )
    }

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
