package aptus.utils

import aptus.{Seq_, String_}

// ===========================================================================
object SeqUtils {

  def distinct[A](a: Seq[A], f: (Boolean, Any) => Unit): Seq[A] = {
    f(
      a.isDistinct,
      (a.size, a.duplicates.#@@.sectionAllOff(2)))

    a
  }

  // ---------------------------------------------------------------------------
  def requireDistinctBy[A, B](coll: Seq[A])(f: A => B): Seq[A] = {
    val x = coll.map(f)

    require(
        x.isDistinct,
        x.duplicates.#@@.sectionAllOff(2))

    coll
  }

}

// ===========================================================================
