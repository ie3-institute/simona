/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkovParamStoreSpec extends AnyFlatSpec with Matchers {

  "dish_washer" should "return a map of Season_Day Category's and their corresponding probabilities" in {
    val probabilitiesMap = MarkovParamStore.Usage_Probabilities()
    probabilitiesMap shouldBe a[Map[_, _]]
    probabilitiesMap.size shouldEqual 12
  }

  "usage_probabilities" should "return a map of appliance Category's and their corresponding probabilities" in {
    val probabilitiesMap = MarkovParamStore.Usage_Probabilities()
    probabilitiesMap shouldBe a[Map[_, _]]
    probabilitiesMap.size shouldEqual 12
    probabilitiesMap.getOrElse("other_load", 0.0) shouldEqual 1
  }

  "average_hh" should "return a map of appliances and their corresponding probabilities" in {
    val average_HHMap = MarkovParamStore.Average_HH()
    average_HHMap shouldBe a[Map[_, _]]
    average_HHMap.size shouldEqual 13
    average_HHMap.getOrElse("lighting", 0.0) shouldEqual 2.5
  }

  "by_type" should "return a map of appliances in a House or Flat and their corresponding probabilities" in {
    val TypeMap = MarkovParamStore.Type()
    TypeMap shouldBe a[scala.collection.mutable.Map[_, _]]
    TypeMap.size shouldEqual 2
  }

  "by_income" should "return a map of appliances per income and their corresponding probabilities" in {
    val incomeMap = MarkovParamStore.income()
    incomeMap shouldBe a[scala.collection.mutable.Map[_, _]]
    incomeMap.size shouldEqual 8
  }

  "by_inhabitants" should "return a map of appliances per inhabitants and their corresponding probabilities" in {
    val inhabitantsMap = MarkovParamStore.inhabitants()
    inhabitantsMap shouldBe a[scala.collection.mutable.Map[_, _]]
    inhabitantsMap.size shouldEqual 5
  }

  "load_ts" should "return a map of appliances and their corresponding Load Time Series" in {
    val load_TSMap = MarkovParamStore.load_TS()
    load_TSMap shouldBe a[scala.collection.mutable.Map[_, _]]
    load_TSMap.size shouldEqual 13
  }

}
