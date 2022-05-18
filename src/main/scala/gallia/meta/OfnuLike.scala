package gallia
package meta

// ===========================================================================
trait Info1Like
  extends HasOptionalAndMultiple
     with HasSingleValueType {
  protected val _info1: Info1

  // ===========================================================================
  def subInfo: SubInfo = SubInfo(multiple, valueType)

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneString  : Boolean = isOne && isString
  def isOneInt     : Boolean = isOne && isInt
  def isOneDouble  : Boolean = isOne && isDouble
  def isOneBoolean : Boolean = isOne && isBoolean
}

// ===========================================================================
