package gallia
package meta

// ===========================================================================
case class Info1(
        optional : Optional,
        multiple : Multiple,
        valueType: ValueType)
      extends Info1Like {
    final override protected val _info1: Info1 = this

    // ---------------------------------------------------------------------------
    override def toString = formatDefault

      def formatDefault: String =
        //  see t210125111338 (union types)
        if (optional) s"${formatOptional(optional)}\t${subInfo.formatDefault}"
        else                                           subInfo.formatDefault

    // ===========================================================================
    def toRequired: Info1 = copy(optional = false)
    def toOptional: Info1 = copy(optional = true)

    def toSingle  : Info1 = copy(multiple = false)
    def toMultiple: Info1 = copy(multiple = true)
  }

  // ===========================================================================
  object Info1 {
    def one(valueType: ValueType): Info1 = Info1(_Required, _Single  , valueType)
    def opt(valueType: ValueType): Info1 = Info1(_Optional, _Single, valueType)
    def nes(valueType: ValueType): Info1 = Info1(_Required, _Multiple, valueType)
    def pes(valueType: ValueType): Info1 = Info1(_Optional, _Multiple, valueType)

    // ---------------------------------------------------------------------------
    def one(basic: BasicType.type => BasicType): Info1 = Info1(_Required, _Single  , basic(BasicType))
    def opt(basic: BasicType.type => BasicType): Info1 = Info1(_Optional, _Single  , basic(BasicType))
    def nes(basic: BasicType.type => BasicType): Info1 = Info1(_Required, _Multiple, basic(BasicType))
    def pes(basic: BasicType.type => BasicType): Info1 = Info1(_Optional, _Multiple, basic(BasicType))

    // ---------------------------------------------------------------------------
    def oneBoolean: Info1 = one(_._Boolean)
    def oneInt    : Info1 = one(_._Int)
    def oneDouble : Info1 = one(_._Double)
    def oneString : Info1 = one(_._String)

    // ---------------------------------------------------------------------------
    def optBoolean: Info1 = opt(_._Boolean)
    def optInt    : Info1 = opt(_._Int)
    def optDouble : Info1 = opt(_._Double)
    def optString : Info1 = opt(_._String)
  }

// ===========================================================================
