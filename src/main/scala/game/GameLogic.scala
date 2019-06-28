package game

import cards.Card

sealed trait Outcome

case object Player extends Outcome
case object Banker extends Outcome
case object Tie extends Outcome

case class GameResult(outcome: Outcome, playerResult: Int, bankerResult: Int, shoe: List[Card])

object GameLogic {
  final def dealHand(shoe: List[Card]): GameResult = {
    val firstCard = shoe.head
    val secondCard = shoe(1)
    val thirdCard = shoe(2)
    val fourthCard = shoe(3)

    val playerHand = (firstCard, thirdCard)
    val bankerHand = (secondCard, fourthCard)
    val playerInitialScore = (playerHand._1.rank.value + playerHand._2.rank.value) % 10
    val bankerInitialScore = (bankerHand._1.rank.value + bankerHand._2.rank.value) % 10

    (playerInitialScore, bankerInitialScore) match {
      case (9, 9) => GameResult(Tie, 9, 9, shoe.drop(4))
      case (9, banker) => GameResult(Player, 9, banker, shoe.drop(4))
      case (player, 9) => GameResult(Banker, player, 9, shoe.drop(4))
      case (8, 8) => GameResult(Tie, 8, 8, shoe.drop(4))
      case (8, banker) => GameResult(Player, 8, banker, shoe.drop(4))
      case (player, 8) => GameResult(Banker, player, 8, shoe.drop(4))
      case (7, 7) => GameResult(Tie, 7, 7, shoe.drop(4))
      case (7, 6) => GameResult(Player, 7, 6, shoe.drop(4))
      case (6, 7) => GameResult(Banker, 6, 7, shoe.drop(4))
      case (7, banker) =>
        val bankerFinalScore = (banker + shoe(4).rank.value) % 10

        bankerFinalScore match {
          case 9 => GameResult(Banker, 7, 9, shoe.drop(5))
          case 8 => GameResult(Banker, 7, 8, shoe.drop(5))
          case 7 => GameResult(Tie, 7, 7, shoe.drop(5))
          case _ => GameResult(Player, 7, bankerFinalScore, shoe.drop(5))
        }
      case (player, 7) =>
        val playerFinalScore = (player + shoe(4).rank.value) % 10

        playerFinalScore match {
          case 9 => GameResult(Player, 9, 7, shoe.drop(5))
          case 8 => GameResult(Player, 8, 7, shoe.drop(5))
          case 7 => GameResult(Tie, 7, 7, shoe.drop(5))
          case _ => GameResult(Banker, playerFinalScore, 7, shoe.drop(5))
        }
      case (6, 6) => GameResult(Tie, 6, 6, shoe.drop(4))
    }
  }
}
