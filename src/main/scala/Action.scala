package com.citylosers

abstract class Action

abstract class PlayAction extends Action
case class AddToExpeditionAction(expeditionIndex: Int, cardIndex: Int) extends PlayAction
case class DiscardCardAction(cardIndex: Int) extends PlayAction

abstract class DrawAction extends Action
case class DeckDrawAction extends PlayAction
case class DiscardPileDrawAction(pileIndex: Int) extends PlayAction

case class Turn(playAction: PlayAction, drawAction: DrawAction)
