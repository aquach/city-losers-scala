import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import com.citylosers.Card
import com.citylosers.ExpeditionCard
import com.citylosers.InvestmentCard
import com.citylosers.Deck

object DeckSpecification extends Properties("Deck") {

  def newDeck = Gen.resultOf[Unit, Seq[Card]](_ => Deck.newDeck())

  property("isCorrectSize") = forAll(newDeck) { (d: Seq[Card]) =>
    d.size == Deck.N_DESTINATIONS * (Deck.N_INVESTMENTS_PER_DESTINATION + Deck.N_EXPEDITIONS_PER_DESTINATION)
  }

  property("hasCorrectNumberOfInvestmentCards") = forAll(newDeck) { (d: Seq[Card]) =>
    (d.filter(_.isInstanceOf[InvestmentCard])).size == Deck.N_DESTINATIONS * Deck.N_INVESTMENTS_PER_DESTINATION 
  }

  property("hasCorrectNumberOfExpeditionCards") = forAll(newDeck) { (d: Seq[Card]) =>
    (d.filter(_.isInstanceOf[ExpeditionCard])).size == Deck.N_DESTINATIONS * Deck.N_EXPEDITIONS_PER_DESTINATION 
  }

  property("isAlwaysRandom") = forAll(newDeck, newDeck) { (d1: Seq[Card], d2: Seq[Card]) =>
    d1 != d2
  }
}
