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
      case (6 | 7 | 8 | 9, 6 | 7 | 8 | 9) =>
        compareHands(
          playerInitialScore,
          playerInitialHand,
          bankerInitialScore,
          bankerInitialHand,
          shoe.drop(4))
      case (6 | 7, _) =>
        val (bankerFinalScore, bankerFinalHand) = hit(bankerInitialHand, shoe(4))
        compareHands(
          playerInitialScore,
          playerInitialHand,
          bankerFinalScore,
          bankerFinalHand,
          shoe.drop(5))
      case (_, _) =>
        val playerHitCard = shoe(4)
        val (playerFinalScore, playerFinalHand) = hit(playerInitialHand, playerHitCard)

        if (`hitBankHand?`(bankerInitialScore, playerHitCard.rank.value)) {
          val (bankerFinalScore, bankerFinalHand) = hit(bankerInitialHand, shoe(5))
          compareHands(
            playerFinalScore,
            playerFinalHand,
            bankerFinalScore,
            bankerFinalHand,
            shoe.drop(6))
        } else
          compareHands(
            playerFinalScore,
            playerFinalHand,
            bankerInitialScore,
            bankerInitialHand,
            shoe.drop(5))
    }
  }

  private final def hit(hand: List[Card], card: Card): (Int, List[Card]) = {
    val finalHand = hand ++ List(card)
    val finalScore = (finalHand.head.rank.value + finalHand(1).rank.value + finalHand(2).rank.value) % 10

    (finalScore, finalHand)
  }

  private final def `hitBankHand?`(bankerInitialScore: Int, playerHitCardValue: Int): Boolean =
    bankerInitialScore match {
      case 7 => false
      case 6 => if (playerHitCardValue == 6 || playerHitCardValue == 7) true else false
      case 5 => if (playerHitCardValue >= 4 && playerHitCardValue <= 7) true else false
      case 4 => if (playerHitCardValue >= 2 && playerHitCardValue <= 7) true else false
      case 3 => if (playerHitCardValue != 8) true else false
      case _ => true
    }

  private final def compareHands(
      playerScore: Int,
      playerHand: List[Card],
      bankerScore: Int,
      bankerHand: List[Card],
      shoe: List[Card]): GameResult =
    if (playerScore > bankerScore)
      GameResult(Player, playerScore, playerHand, bankerScore, bankerHand, shoe)
    else if (playerScore < bankerScore)
      GameResult(Banker, playerScore, playerHand, bankerScore, bankerHand, shoe)
    else GameResult(Tie, playerScore, playerHand, bankerScore, bankerHand, shoe)
}
