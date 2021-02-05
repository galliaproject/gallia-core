package gallia.meta

// ===========================================================================
@aptus.pseudosealed /* only two: BasicType and Cls (nesting) */ trait Containee {
    private def _either: Either[BasicType, Cls] = this match {
      case x: BasicType => Left (x)
      case x: Cls       => Right(x) }

    // ---------------------------------------------------------------------------
    def nestingOpt: Option[Cls      ] = _either     .toOption
    def leafOpt   : Option[BasicType] = _either.swap.toOption
  }

  // ===========================================================================
  object Containee {

    // TODO: Containee.combine: t201124151910; note for union, data would also need to be transformed (int/double)
    @gallia.NumberAbstraction
    def combine(c1: Containee, c2: Containee): Containee = {
      c1 // FIXME
    }

  }

// ===========================================================================
