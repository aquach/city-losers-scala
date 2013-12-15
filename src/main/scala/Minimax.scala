package com.citylosers

class Minimax[StateType](
    positionHeuristic: StateType => Int,
    computeNextStates: StateType => Seq[StateType],
    maxDepth: Int) {

  def minimax(currentState: StateType, forPositivePlayer: Boolean): (StateType, Int) =
    minimax(currentState, maxDepth, forPositivePlayer)

  def aminimax(currentState: StateType, forPositivePlayer: Boolean): (StateType, Int) =
    aminimax(currentState, maxDepth, forPositivePlayer, Int.MinValue, Int.MaxValue)

  private def aminimax(currentState: StateType, depth: Int, 
      shouldMax: Boolean, alpha: Int, beta: Int): (StateType, Int) =
    if (depth <= 0) {
      (currentState, positionHeuristic(currentState))
    } else {
      var bestMove = null.asInstanceOf[StateType]
      var cutoffVar = if (shouldMax) alpha else beta

      computeNextStates(currentState).toIterator.takeWhile(_ => {
        if (shouldMax) cutoffVar < beta else alpha < cutoffVar
      }).foreach(possibleState => {
        val (tmpA, tmpB) = if (shouldMax) (cutoffVar, beta) else (alpha, cutoffVar)

        val result = aminimax(possibleState, depth - 1, !shouldMax, tmpA, tmpB)

        val cmp = if (shouldMax) math.max _ else math.min _
        cutoffVar = cmp(cutoffVar, result._2)
      })

      (bestMove, cutoffVar)
    }

  private def minimax(currentState: StateType, depth: Int, 
      shouldMax: Boolean): (StateType, Int) =
    if (depth <= 0) {
      (currentState, positionHeuristic(currentState))
    } else {
      val moves = computeNextStates(currentState).toIterator.map { nextState =>
         minimax(nextState, depth - 1, !shouldMax)
      }

      if (shouldMax) moves.maxBy(_._2) else moves.minBy(_._2)
    }
}
