package gallia

// ===========================================================================
object GalliaUtils { // consider moving those to aptus whenever generic enough

  implicit class Seq__[T](values: Seq[T]) {

    def mapAffectExactlyOne(pred: T => Boolean)(f: T => T): Seq[T] = {
      var encountered = 0
      val newValues =
        values
          .map { value =>
            if (!pred(value)) value
            else {
              encountered += 1
              f(value) }}
      assert(encountered == 1, values)

      newValues
    }

  }

  // ===========================================================================
  private[gallia] trait GalliaSeq[T] { //TODO: to aptus if generalizes well?
    protected val seq: Seq[T]

    // ---------------------------------------------------------------------------
    def ifOne[U](ifSole: T => U, otherwise: Seq[T] => U): U = ifOneMatch(_ => true)(ifSole, otherwise)

    // ---------------------------------------------------------------------------
    def ifOneMatch[U](pred: T => Boolean)(ifSole: T => U, otherwise: Seq[T] => U): U = ifOneMatchEither(pred) match {
      case Right(x) => ifSole   (x)
      case Left (x) => otherwise(x) }

    // ---------------------------------------------------------------------------
    def ifOneMatchEither(pred: T => Boolean): Either[Seq[T], T] =
      seq.filter(pred) match {
        case Seq(sole) => Right(sole)
        case other     => Left (other) }
  }

}

// ===========================================================================
