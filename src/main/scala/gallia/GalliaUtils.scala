package gallia

// ===========================================================================
object GalliaUtils { // consider moving those to aptus whenever generic enough

  implicit class Seq__[T](values: Seq[T]) {

    def mapIfGuaranteed(pred: T => Boolean)(f: T => T): Seq[T] = {
      var encountered = false
      val newValues =
        values
          .map { value =>
            if (!pred(value)) value
            else {
              encountered = true
              f(value) }}
      assert(encountered, values)

      newValues
    }

  }

}

// ===========================================================================
