/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

object RunPrinting extends LazyLogging {
  // a fancy opener
  def printOpener(): Unit = {
    println(
      "   _____ ______  _______  _   _____       ___    ____ \n  / ___//  _/  |/  / __ \\/ | / /   |     |__ \\  / __ \\\n  \\__ \\ / // /|_/ / / / /  |/ / /| |     __/ / / / / /\n ___/ // // /  / / /_/ / /|  / ___ |    / __/_/ /_/ / \n/____/___/_/  /_/\\____/_/ |_/_/  |_|   /____(_)____/  \n                                                      "
    )
  }

  def printGoodbye(): Unit = {
    val myWords = Array(
      "\"Vielleicht ist heute ein besonders guter Tag zum Sterben.\" - Worf (in Star Trek: Der erste Kontakt)",
      "\"Assimiliert das!\" - Worf (in Star Trek: Der erste Kontakt)",
      "\"Lebe lang und erfolgreich.\" - Gruppe von Vulkanier (in Star Trek: Der erste Kontakt)",
      "\"Ich bin der Anfang, das Ende, die Eine, die Viele ist. Ich bin die Borg.\" - Borg-Königin (in Star Trek: Der erste Kontakt)",
      "\"A horse! A horse! My kingdom for a horse!\" - King Richard III (in Shakespeare's Richard III, 1594)"
    )

    val rand = new Random
    val randIdx = rand.nextInt(myWords.length)
    logger.info(myWords(randIdx))
    logger.info("Goodbye!")
  }
}
