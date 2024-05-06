import edu.ie3.simona.model.participant.load.markov.MarkovParamStore
import edu.ie3.simona.model.participant.load.markov.MarkovParamStore.{SOP, Usage_Probabilities, getDefaultReaderForSOP}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkovParamStoreSpec extends AnyFlatSpec with Matchers {

  "A Map" should "have the correct size" in {
    val map = Usage_Probabilities()
    map.size should be(2)
  }


}
