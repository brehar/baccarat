package cards

import scala.util.Random

object Shoe {
  final val NUMBER_OF_DECKS: Int = 8
  final val NUMBER_DECKS_UNUSED: Double = 0.5

  final def generate(): List[Card] =
    (for (_ <- 1 to NUMBER_OF_DECKS) yield Card.generateDeck()).flatten.toList

  final def shuffle(shoe: List[Card]): List[Card] = Random.shuffle(shoe)
}
