/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import java.io.{InputStreamReader, Reader}
import java.time.{Duration, ZonedDateTime}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.exceptions.FileIOException
import edu.ie3.simona.model.participant.load.DayType // Use for Markov too

import edu.ie3.simona.model.participant.load.markov.SwitchOnProbabilityKey
import org.apache.commons.csv.{CSVFormat, CSVRecord}

import scala.jdk.CollectionConverters._

// appliances




// probabilities
