package gallia
package meta

// ===========================================================================
case class Ofnu(
        optional : Optional,
        multiple : Multiple,
        containee: Containee)
      extends OfnuLike {
    final override protected val _ofnu: Ofnu = this

    // ---------------------------------------------------------------------------
    override def toString = formatDefault

      def formatDefault: String =
        //  see t210125111338 (union types)
        if (optional) s"${formatOptional(optional)}\t${info.formatDefault}"
        else                                           info.formatDefault

    // ===========================================================================
    def toRequired: Ofnu = copy(optional = false)
    def toOptional: Ofnu = copy(optional = true)

    def toSingle  : Ofnu = copy(multiple = false)
    def toMultiple: Ofnu = copy(multiple = true)
  }

  // ===========================================================================
  object Ofnu {
    def one(containee: Containee): Ofnu = Ofnu(_Required, _Single  , containee)
    def opt(containee: Containee): Ofnu = Ofnu(_Optional, _Single, containee)
    def nes(containee: Containee): Ofnu = Ofnu(_Required, _Multiple, containee)
    def pes(containee: Containee): Ofnu = Ofnu(_Optional, _Multiple, containee)

    // ---------------------------------------------------------------------------
    def one(basic: BasicType.type => BasicType): Ofnu = Ofnu(_Required, _Single  , basic(BasicType))
    def opt(basic: BasicType.type => BasicType): Ofnu = Ofnu(_Optional, _Single  , basic(BasicType))
    def nes(basic: BasicType.type => BasicType): Ofnu = Ofnu(_Required, _Multiple, basic(BasicType))
    def pes(basic: BasicType.type => BasicType): Ofnu = Ofnu(_Optional, _Multiple, basic(BasicType))

    // ---------------------------------------------------------------------------
    def oneBoolean: Ofnu = one(_._Boolean)
    def oneInt    : Ofnu = one(_._Int)
    def oneDouble : Ofnu = one(_._Double)
    def oneString : Ofnu = one(_._String)

    // ---------------------------------------------------------------------------
    def optBoolean: Ofnu = opt(_._Boolean)
    def optInt    : Ofnu = opt(_._Int)
    def optDouble : Ofnu = opt(_._Double)
    def optString : Ofnu = opt(_._String)
  }

// ===========================================================================
