package baccarat

import java.text.NumberFormat

import cards.{ Card, Shoe }

class State(
    val shoe: List[Card],
    val nrPlayer: Int = 0,
    val nrBanker: Int = 0,
    val nrTie: Int = 0,
    val bankroll: Double = 1000.00) {
  private final val formatter = NumberFormat.getCurrencyInstance

  def showStatistics(): Unit = {
    println("–" * 50)
    println(s"Bankroll   : ${formatter.format(bankroll)}")
    println(s"Hands Dealt: ${nrPlayer + nrBanker + nrTie}")
    println(s"Player Wins: $nrPlayer")
    println(s"Banker Wins: $nrBanker")
    println(s"Number Ties: $nrTie")
    println(s"There are $cardsRemaining cards in the shoe until the cutting card.")
    println("–" * 50)
  }

  final def shoeOver: Boolean = cardsRemaining < 0

  final private def cardsRemaining: Int = (shoe.length - Shoe.NUMBER_DECKS_UNUSED * 52).toInt
}

object State {
  def apply(): State = new State(Shoe.shuffle(Shoe.generate()))
}
