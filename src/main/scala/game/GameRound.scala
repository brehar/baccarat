package game

import baccarat.State

import scala.io.StdIn

object GameRound {
  final def run(state: State): State = {
    state.showStatistics()

    val wager = getWager(state.bankroll)
    val role = getRole
    val tieBet = `tieBet?`
    val tieBetAmount =
      if (tieBet) getWager(state.bankroll - wager)
      else 0

    println("No more bets. Dealing...")

    val GameResult(outcome, playerResult, playerHand, bankerResult, bankerHand, shoe) =
      GameLogic.dealHand(state.shoe)

    println(
      s"Player shows ${(playerHand.head.rank.value + playerHand(1).rank.value) % 10}: ${playerHand.head} and ${playerHand(1)}")

    Thread.sleep(1000)

    println(
      s"Banker shows ${(bankerHand.head.rank.value + bankerHand(1).rank.value) % 10}: ${bankerHand.head} and ${bankerHand(1)}")

    Thread.sleep(1000)

    if (playerHand(2) != null) {
      println(s"Extra card for the player: ${playerHand(2)}")
      println(s"Player now shows $playerResult")
      Thread.sleep(1000)
    }

    if (bankerHand(2) != null) {
      println(s"Extra card for the bank: ${bankerHand(2)}")
      println(s"Banker now shows $bankerResult")
      Thread.sleep(1000)
    }

    println(s"$outcome wins!")

    outcome match {
      case Banker =>
        if (role == Banker)
          new State(
            shoe,
            state.nrPlayer,
            state.nrBanker + 1,
            state.nrTie,
            state.bankroll + 0.95 * wager - tieBetAmount)
        else
          new State(
            shoe,
            state.nrPlayer,
            state.nrBanker + 1,
            state.nrTie,
            state.bankroll - wager - tieBetAmount)
      case Player =>
        if (role == Player)
          new State(
            shoe,
            state.nrPlayer + 1,
            state.nrBanker,
            state.nrTie,
            state.bankroll + wager - tieBetAmount)
        else
          new State(
            shoe,
            state.nrPlayer + 1,
            state.nrBanker,
            state.nrTie,
            state.bankroll - wager - tieBetAmount)
      case Tie =>
        if (tieBet)
          new State(
            shoe,
            state.nrPlayer,
            state.nrBanker,
            state.nrTie + 1,
            state.bankroll + tieBetAmount * 8)
        else new State(shoe, state.nrPlayer, state.nrBanker, state.nrTie + 1, state.bankroll)
    }
  }

  @scala.annotation.tailrec
  private final def getWager(bankroll: Double): Double = {
    print("Enter your wager: ")
    val wager = StdIn.readDouble()

    if (wager > bankroll) {
      println(s"You cannot bet more than your bankroll of $bankroll.")
      getWager(bankroll)
    } else wager
  }

  @scala.annotation.tailrec
  private final def getRole: Outcome = {
    print("Select (P)layer or (B)anker: ")
    val role = StdIn.readLine()

    if (role.trim.toUpperCase == "B") Banker
    else if (role.trim.toUpperCase == "P") Player
    else {
      println("Invalid selection. Please try again.")
      getRole
    }
  }

  @scala.annotation.tailrec
  private final def `tieBet?` : Boolean = {
    print("Would you like to bet the tie? (Y/N) ")
    val tieBet = StdIn.readLine()

    if (tieBet.trim.toUpperCase == "Y") true
    else if (tieBet.trim.toUpperCase == "N") false
    else {
      println("Invalid selection. Please try again.")
      `tieBet?`
    }
  }
}
