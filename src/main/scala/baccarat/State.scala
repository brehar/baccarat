package baccarat

import cards.{ Card, Shoe }

class State(val shoe: List[Card], val nrPlayer: Int = 0, val nrBanker: Int = 0, nrTie: Int = 0) {
  def showStatistics(): Unit = {
    println("–" * 50)
    println(s"Hands Dealt: ${nrPlayer + nrBanker + nrTie}")
    println(s"Player Wins: $nrPlayer")
    println(s"Banker Wins: $nrBanker")
    println(s"Number Ties: $nrTie")
    println(s"There are $cardsRemaining cards in the shoe until the cutting card.")
    println("–" * 50)
  }

  private def cardsRemaining: Int = (shoe.length - Shoe.NUMBER_DECKS_UNUSED * 52).toInt

  private def shoeOver: Boolean = cardsRemaining <= 0
}

object State {
  def apply(): State = new State(Shoe.shuffle(Shoe.generate()))
}
