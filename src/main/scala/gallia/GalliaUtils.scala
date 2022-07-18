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
      assert(encountered == 1, encountered -> values)

      newValues
    }

  }

  // ===========================================================================
  private[gallia] trait GalliaSeq[T] { //TODO: to aptus if generalizes well?
    protected val seq: Seq[T]

    // ---------------------------------------------------------------------------
    def mapIndex(index: aptus.Index)(f: T => T): Seq[T] = { var i = -1; seq.map { x => i += 1; if (i == index) f(x) else x } } // TODO: confirm faster than zipWithIndex?

    // ===========================================================================
    def ifOneElementOpt: Option[T] = if (seq.size == 1) Some(seq.head) else None

    // ---------------------------------------------------------------------------
    def ifOneElement[U](ifOne: T => U, otherwise: Seq[T] => U): U =
      if (seq.size == 1) ifOne    (seq.head)
      else               otherwise(seq)

    // ---------------------------------------------------------------------------
    def ifOneElementOrElse(errorMessage: Seq[T] => Any): T =
      if (seq.size == 1) seq.head
      else               aptus.illegalState(seq.size, errorMessage(seq))
  }

}

// ===========================================================================
