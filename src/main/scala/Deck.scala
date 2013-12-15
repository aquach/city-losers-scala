package com.citylosers

import scala.util.Random

abstract class Card {
  val destination: Int
}
case class InvestmentCard(destination: Int) extends Card
case class ExpeditionCard(destination: Int, rank: Int) extends Card

object Deck {
  val N_DESTINATIONS = 5
  val N_INVESTMENTS_PER_DESTINATION = 3
  val N_EXPEDITIONS_PER_DESTINATION = 9
  def newDeck(): Seq[Card] = 
    Random.shuffle((for (dest <- 0 until N_DESTINATIONS) yield {
      (0 until N_EXPEDITIONS_PER_DESTINATION).map(e => ExpeditionCard(dest, e)) ++
      (0 until N_INVESTMENTS_PER_DESTINATION).map(i => InvestmentCard(dest))
    }).flatten)
}
