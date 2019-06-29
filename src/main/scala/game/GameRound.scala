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

    if (role.toUpperCase == "B") Banker
    else if (role.toUpperCase == "P") Player
    else {
      println("Invalid selection. Please try again.")
      getRole
    }
  }

  @scala.annotation.tailrec
  private final def `tieBet?` : Boolean = {
    print("Would you like to bet the tie? (Y/N) ")
    val tieBet = StdIn.readLine()

    if (tieBet.toUpperCase == "Y") true
    else if (tieBet.toUpperCase == "N") false
    else {
      println("Invalid selection. Please try again.")
      `tieBet?`
    }
  }
}
