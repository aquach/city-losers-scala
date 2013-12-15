package com.citylosers

case class GameState(players: Seq[Player], nextPlayerIndex: Int, deck: Seq[Card], discardPiles: Map[Int, Seq[Card]]) {
  private val nextPlayer = players(nextPlayerIndex)
  private val nextNextPlayerIndex = (nextPlayerIndex + 1) % GameState.N_PLAYERS

  def doTurn(t: Turn): Option[GameState] = t match {
    case Turn(playAction, drawAction) => doPlay(playAction).flatMap(_.doDraw(drawAction))
  }

  private def removeIndex[T](l: Seq[T], i: Int): (T, Seq[T]) = {
    val (first, last) = l.splitAt(i)

    (last.head, first ++ last.tail)
  }

  private def doPlay(p: PlayAction): Option[GameState] = p match {
    case AddToExpeditionAction(expeditionIndex, cardIndex) => None
    case DiscardCardAction(cardIndex) => {
      val (card, newHand) = removeIndex(nextPlayer.hand, cardIndex)
      val newPlayer = Player(newHand, nextPlayer.expeditions)
      Some(GameState(
        players.updated(nextPlayerIndex, newPlayer),
        nextNextPlayerIndex,
        deck,
        discardPiles.updated(card.destination, card +: discardPiles(card.destination))
      ))
    }
  }

  private def doDraw(d: DrawAction): Option[GameState] = Some(this)
}

object GameState {
  val STARTING_CARDS = 8
  val N_PLAYERS = 2
  def newGame() : GameState = {
    val (deck, players) = (0 until N_PLAYERS).foldLeft((Deck.newDeck(), Seq[Player]()))({ (data, _) => data match {
      case (deck, players) => 
        val (hand, rest) = deck.splitAt(STARTING_CARDS)
        (rest, Player(hand, Map[Int, Seq[Card]]()) +: players)
    }})

    GameState(
      players,
      0,
      deck,
      Map[Int, Seq[Card]]()
    )
  }
}
