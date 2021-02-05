package aptus

// ===========================================================================
private[aptus] final class As[A] private[aptus](private val a: A) {
  def some: Option[A]  = Some(a)
  def seq : Seq   [A]  = Seq(a)
  def list: List  [A]  = List(a)

  // ---------------------------------------------------------------------------
  def left [$Right]: Either[A, $Right] = Left(a)
  def right[$Left ]: Either[$Left, A]  = Right(a)

  // ---------------------------------------------------------------------------
  def noneIf(pred: A => Boolean): Option[A] = if ( pred(a)) None else Some(a)
  def someIf(pred: A => Boolean): Option[A] = if (!pred(a)) None else Some(a)
}

// ===========================================================================
