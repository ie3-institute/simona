import edu.ie3.simona.model.participant.load.markov.MarkovParamStore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkovParamStoreSpec extends AnyFlatSpec with Matchers {

    "usage_probabilities" should "return a map of appliance Category's and their corresponding probabilities" in {
      val probabilitiesMap = MarkovParamStore.Usage_Probabilities()
      probabilitiesMap shouldBe a[Map[_, _]]
      probabilitiesMap.size shouldEqual 12
      probabilitiesMap.getOrElse("other_load", 0.0) shouldEqual 1
    }

      "average_hh" should "return a map of appliances and their corresponding probabilities" in {
        val average_HHMap = MarkovParamStore.Average_HH()
        average_HHMap shouldBe a [Map[_, _]]
        average_HHMap.size shouldEqual 13
        average_HHMap.getOrElse("lighting", 0.0) shouldEqual 2.5
    }
  }
