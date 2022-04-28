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
