package game

import cards.Card

sealed trait Outcome

case object Player extends Outcome
case object Banker extends Outcome
case object Tie extends Outcome

case class GameResult(
    outcome: Outcome,
    playerResult: Int,
    playerHand: List[Card],
    bankerResult: Int,
    bankerHand: List[Card],
    shoe: List[Card])

object GameLogic {
  final def dealHand(shoe: List[Card]): GameResult = {
    val firstCard = shoe.head
    val secondCard = shoe(1)
    val thirdCard = shoe(2)
    val fourthCard = shoe(3)

    val playerInitialHand = List(firstCard, thirdCard)
    val bankerInitialHand = List(secondCard, fourthCard)
    val playerInitialScore = (playerInitialHand.head.rank.value + playerInitialHand(1).rank.value) % 10
    val bankerInitialScore = (bankerInitialHand.head.rank.value + bankerInitialHand(1).rank.value) % 10

    (playerInitialScore, bankerInitialScore) match {
      case (9, 9) => GameResult(Tie, 9, playerInitialHand, 9, bankerInitialHand, shoe.drop(4))
      case (9, _) =>
        GameResult(
          Player,
          9,
          playerInitialHand,
          bankerInitialScore,
          bankerInitialHand,
          shoe.drop(4))
      case (_, 9) =>
        GameResult(
          Banker,
          playerInitialScore,
          playerInitialHand,
          9,
          bankerInitialHand,
          shoe.drop(4))
      case (8, 8) => GameResult(Tie, 8, playerInitialHand, 8, bankerInitialHand, shoe.drop(4))
      case (8, _) =>
        GameResult(
          Player,
          8,
          playerInitialHand,
          bankerInitialScore,
          bankerInitialHand,
          shoe.drop(4))
      case (_, 8) =>
        GameResult(
          Banker,
          playerInitialScore,
          playerInitialHand,
          8,
          bankerInitialHand,
          shoe.drop(4))
      case (7, 7) => GameResult(Tie, 7, playerInitialHand, 7, bankerInitialHand, shoe.drop(4))
      case (7, 6) => GameResult(Player, 7, playerInitialHand, 6, bankerInitialHand, shoe.drop(4))
      case (6, 7) => GameResult(Banker, 6, playerInitialHand, 7, bankerInitialHand, shoe.drop(4))
      case (7, _) =>
        val (bankerFinalScore, bankerFinalHand) = hit(bankerInitialHand, shoe(4))

        bankerFinalScore match {
          case x @ (8 | 9) =>
            GameResult(Banker, 7, playerInitialHand, x, bankerFinalHand, shoe.drop(5))
          case 7 => GameResult(Tie, 7, playerInitialHand, 7, bankerFinalHand, shoe.drop(5))
          case _ =>
            GameResult(
              Player,
              7,
              playerInitialHand,
              bankerFinalScore,
              bankerFinalHand,
              shoe.drop(5))
        }
      case (_, 7) =>
        val (playerFinalScore, playerFinalHand) = hit(playerInitialHand, shoe(4))

        playerFinalScore match {
          case x @ (8 | 9) =>
            GameResult(Player, x, playerFinalHand, 7, bankerInitialHand, shoe.drop(5))
          case 7 => GameResult(Tie, 7, playerFinalHand, 7, bankerInitialHand, shoe.drop(5))
          case _ =>
            GameResult(
              Banker,
              playerFinalScore,
              playerFinalHand,
              7,
              bankerInitialHand,
              shoe.drop(5))
        }
      case (6, 6) => GameResult(Tie, 6, playerInitialHand, 6, bankerInitialHand, shoe.drop(4))
    }
  }

  private def hit(hand: List[Card], card: Card): (Int, List[Card]) = {
    val finalHand = hand ++ List(card)
    val finalScore = (finalHand.head.rank.value + finalHand(1).rank.value + finalHand(2).rank.value) % 10

    (finalScore, finalHand)
  }
}
