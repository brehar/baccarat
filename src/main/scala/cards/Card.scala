package cards

case class Card(rank: Rank, suit: Suit) {
  final override def toString: String = s"$rank of $suit"
}

object Card {
  final val ranks: List[Rank] = List(
    Ace(),
    Two(),
    Three(),
    Four(),
    Five(),
    Six(),
    Seven(),
    Eight(),
    Nine(),
    Ten(),
    Jack(),
    Queen(),
    King())
  final val suits: List[Suit] = List(Clubs, Diamonds, Hearts, Spades)

  final def generateDeck(): List[Card] = for (rank <- ranks; suit <- suits) yield Card(rank, suit)
}
