package gallia
package meta

// ===========================================================================
@aptus.pseudosealed /* only two: BasicType and Cls (nesting) */ trait Containee {
    private[gallia] def _either: Either[BasicType, Cls] = this match {
      case x: BasicType => Left (x)
      case x: Cls       => Right(x) }

    // ---------------------------------------------------------------------------
    def isNesting: Boolean = _either.isRight
    def isLeaf   : Boolean = _either.isLeft

    // ---------------------------------------------------------------------------
    def nestingOpt: Option[Cls      ] = _either     .toOption
    def leafOpt   : Option[BasicType] = _either.swap.toOption
    
    // ---------------------------------------------------------------------------
    def forceBasicType: BasicType = this.asInstanceOf[BasicType]
    def forceCls      : Cls       = this.asInstanceOf[Cls]

    // ---------------------------------------------------------------------------
    def isBasicType(pred: BasicType => Boolean): Boolean = leafOpt.exists(pred)
  }

  // ===========================================================================
  object Containee {

    // TODO: Containee.combine: t201124151910; note for union, data would also need to be transformed (int/double)
    @NumberAbstraction
    def combine(c1: Containee, c2: Containee): Containee = {
      c1 // FIXME
    }

  }

// ===========================================================================
