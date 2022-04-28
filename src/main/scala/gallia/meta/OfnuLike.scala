package gallia
package meta

// ===========================================================================
trait OfnuLike
  extends HasOptionalAndMultiple
     with HasSingleContainee {
  protected val _ofnu: Ofnu

  // ===========================================================================
  def info: Info = Info(multiple, containee)

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneString  : Boolean = isOne && isString
  def isOneInt     : Boolean = isOne && isInt
  def isOneDouble  : Boolean = isOne && isDouble
  def isOneBoolean : Boolean = isOne && isBoolean
}

// ===========================================================================
