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
import ch.qos.logback.core.rolling.RollingFileAppender
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.filter.Filter
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory

import java.io.File
import scala.jdk.CollectionConverters._

object LogbackConfiguration extends LazyLogging {

  def default(logPath: String): Unit = {
    LoggerFactory.getILoggerFactory match {
      case loggerContext: LoggerContext =>
        val rootLogger = loggerContext.getLogger("root")
        val log = logPath.concat(File.separator).concat("simona.log")
        // stop all appenders
        rootLogger.iteratorForAppenders().asScala.foreach(_.stop())
        /* Identify the filters of existing rolling file appender */
        val fileLoggerFilterList = rootLogger
          .iteratorForAppenders()
          .asScala
          .find(_.getName == "RF")
          .map(_.getCopyOfAttachedFiltersList.asScala.toSeq)
        rootLogger.addAppender(
          fileAppender(
            log,
            "simona-default",
            fileLoggerFilterList,
            loggerContext,
          )
        )

        rootLogger
          .iteratorForAppenders()
          .asScala
          .foreach(appender => appender.start())

      case factory =>
        logger.error(
          s"Cannot configure simulation run logger! Invalid factory: $factory"
        )
    }

  }

  private def fileAppender(
      logPath: String,
      appenderName: String,
      maybeFilterList: Option[Seq[Filter[ILoggingEvent]]],
      loggerContext: LoggerContext,
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
    /* If applicable, apply the filters from existing file logger else log with "INFO"-Level */
    maybeFilterList match {
      case Some(filterList) =>
        if (filterList.isEmpty) { // No filters in appenders -> Empty List
          val filter = new ThresholdFilter()
          filter.setLevel("INFO")
          filter.start()
          fileAppender.addFilter(filter)
        } else {
          filterList.foreach(fileAppender.addFilter)
        }
    }
    fileAppender.start()

    fileAppender
  }

}
