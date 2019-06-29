package cards

sealed trait Rank {
  val value: Int

  final override def toString: String = this.getClass.getSimpleName
}

final case class Ace(value: Int = 1) extends Rank
final case class Two(value: Int = 2) extends Rank
final case class Three(value: Int = 3) extends Rank
final case class Four(value: Int = 4) extends Rank
final case class Five(value: Int = 5) extends Rank
final case class Six(value: Int = 6) extends Rank
final case class Seven(value: Int = 7) extends Rank
final case class Eight(value: Int = 8) extends Rank
final case class Nine(value: Int = 9) extends Rank
final case class Ten(value: Int = 0) extends Rank
final case class Jack(value: Int = 0) extends Rank
final case class Queen(value: Int = 0) extends Rank
final case class King(value: Int = 0) extends Rank
