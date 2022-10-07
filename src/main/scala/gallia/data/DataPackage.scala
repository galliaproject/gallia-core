package gallia

// ===========================================================================
package object data {
  type BasicType = reflect.BasicType
  val  BasicType = reflect.BasicType

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
