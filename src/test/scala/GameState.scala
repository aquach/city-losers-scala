import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import com.citylosers.Deck
import com.citylosers.GameState
import com.citylosers.Player

object GameStateSpecification extends Properties("GameState") {

  def newGame = Gen.resultOf[Unit, GameState](_ => GameState.newGame)

  property("hasCorrectNumberOfPlayers") = forAll(newGame) { (g: GameState) =>
    g.players.length == GameState.N_PLAYERS
  }

  property("hasNoDiscards") = forAll(newGame) { (g: GameState) =>
    g.discardPiles.size == 0
  }

  property("deckIsSmaller") = forAll(newGame) { (g: GameState) =>
    g.deck.size < Deck.newDeck.size
  }
}

object NewPlayerSpecification extends Properties("NewPlayer") {

  def newPlayer = GameStateSpecification.newGame.flatMap(g => Gen.oneOf(g.players))

  property("hasCorrectHandSize") = forAll(newPlayer) { (p: Player) =>
    p.hand.size == GameState.STARTING_CARDS
  }

  property("hasNoExpeditions") = forAll(newPlayer) { (p: Player) =>
    p.expeditions.size == 0
  }
}

object TurnSpecification extends Properties("Turn") {
  //property("discardCardAction")
}
