package baccarat

import game.GameRound

import scala.io.StdIn

object Baccarat extends App {
  val state = State()

  gameCycle(state)

  @scala.annotation.tailrec
  final private def gameCycle(state: State): Unit = {
    if (state.shoeOver) {
      println(
        s"The shoe has been concluded. Your final bankroll is ${state.bankroll}. Thank you for playing!")
      sys.exit(0)
    }

    val newState = GameRound.run(state)

    if (`playAgain?`) gameCycle(newState)
    else {
      println(s"Your final bankroll is ${newState.bankroll}. Thank you for playing. Goodbye!")
      sys.exit(0)
    }
  }

  @scala.annotation.tailrec
  final private def `playAgain?` : Boolean = {
    print("Would you like to play another round? (Y/N) ")
    val response = StdIn.readLine()

    if (response.trim.toUpperCase == "Y") true
    else if (response.trim.toUpperCase == "N") false
    else {
      println("Invalid response. Please try again.")
      `playAgain?`
    }
  }
}
