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

}

// ===========================================================================
