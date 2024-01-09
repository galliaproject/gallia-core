package gallia
package meta

// ===========================================================================
@aptus.pseudosealed /* only two: BasicType and Cls (nesting) */
trait ValueType extends basic.HasValuePredicate {
    private[gallia] def _either: Either[BasicType, Cls] = this match {
      case x: BasicType => Left (x)
      case x: Cls       => Right(x) }

    // ---------------------------------------------------------------------------
    def formatDefault: String

    // ---------------------------------------------------------------------------
    def isNesting: Boolean = _either.isRight
    def isLeaf   : Boolean = _either.isLeft

    // ---------------------------------------------------------------------------
    def nestingOpt  : Option[Cls      ] = _either     .toOption
    @deprecated
    def leafOpt     : Option[BasicType] = _either.swap.toOption
    def basicTypeOpt: Option[BasicType] = _either.swap.toOption

    // ---------------------------------------------------------------------------
    def forceBasicType: BasicType = this.asInstanceOf[BasicType]
    def forceCls      : Cls       = this.asInstanceOf[Cls]

    // ---------------------------------------------------------------------------
    def isBasicType(pred: BasicType => Boolean): Boolean = basicTypeOpt.exists(pred)
    def isBasicType(target: BasicType)         : Boolean = basicTypeOpt.exists(_ == target)
  }

  // ===========================================================================
  object ValueType {

    // TODO: ValueType.combine: t201124151910; note for union, data would also need to be transformed (int/double)
    @NumberAbstraction
    def combine(c1: ValueType, c2: ValueType): ValueType = {
      c1 // FIXME
    }

  }

// ===========================================================================
