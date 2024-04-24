package edu.ie3.simona.model.participant.load.markov

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MarkovParamStoreSpec extends AnyWordSpec with Matchers {

  "MarkovParamStore" should {
    "print Flat map" in {
      val flatMap = MarkovParamStore.Flat()
      println("Test Function: Flat:")
      flatMap.foreach { case (appliance, value) =>
        println(s"$appliance -> $value")
      }
    }

    "print House map" in {
      val houseMap = MarkovParamStore.House()
      println("Test Function: House:")
      houseMap.foreach { case (appliance, value) =>
        println(s"$appliance -> $value")
      }
    }

    "print Income map" in {
      val incomeMap = MarkovParamStore.income()
      println("Test Function: Income:")
      incomeMap.foreach { case (incomeCategory, appliancesMap) =>
        println(s"Income Category: $incomeCategory")
        appliancesMap.foreach { case (appliance, probability) =>
          println(s"  $appliance -> $probability")
        }
      }
    }

    "print Inhabitants map" in {
      val inhabitantsMap = MarkovParamStore.inhabitants()
      println("Test Function: Inhabitants:")
      inhabitantsMap.foreach { case (inhabitantsCategory, appliancesMap) =>
        println(s"Inhabitants Category: $inhabitantsCategory")
        appliancesMap.foreach { case (appliance, probability) =>
          println(s"  $appliance -> $probability")
        }
      }
    }
  }
}
