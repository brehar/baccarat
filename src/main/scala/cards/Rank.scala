package cards

sealed trait Rank {
  val value: Int

  final override def toString: String = this.getClass.getSimpleName
}

case class Ace(final override val value: Int = 1) extends Rank
case class Two(final override val value: Int = 2) extends Rank
case class Three(final override val value: Int = 3) extends Rank
case class Four(final override val value: Int = 4) extends Rank
case class Five(final override val value: Int = 5) extends Rank
case class Six(final override val value: Int = 6) extends Rank
case class Seven(final override val value: Int = 7) extends Rank
case class Eight(final override val value: Int = 8) extends Rank
case class Nine(final override val value: Int = 9) extends Rank
case class Ten(final override val value: Int = 0) extends Rank
case class Jack(final override val value: Int = 0) extends Rank
case class Queen(final override val value: Int = 0) extends Rank
case class King(final override val value: Int = 0) extends Rank
