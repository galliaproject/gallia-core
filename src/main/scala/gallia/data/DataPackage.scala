package gallia

// ===========================================================================
package object data {
  type BasicType = meta.basic.BasicType
  val  BasicType = meta.basic.BasicType

  type PathPair = domain.PathPair
  val  PathPair = domain.PathPair

  // ---------------------------------------------------------------------------
  private[data] val Base64StringPrefix = "base64:"

  // ---------------------------------------------------------------------------
  /** mostly for Iterator based processing */
  trait DataRegenerationClosure[T] { def regenerate: () => aptus.CloseabledIterator[T] }

    // ---------------------------------------------------------------------------
    object DataRegenerationClosure {
      def fromUncloseable[T](data: Iterator[T]) = new DataRegenerationClosure[T] {
        def regenerate: () => aptus.CloseabledIterator[T] =
          () => aptus.CloseabledIterator.fromUncloseable(data) } }

}

// ===========================================================================
