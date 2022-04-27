package gallia
package meta

import aptus._

// ===========================================================================
trait OfniLike extends InfosLike {
  protected val _ofni: Ofni

  // ===========================================================================
  @deprecated def info1: Info = _ofni.infos match {  // see t210125111338 (union types)
    case Seq(sole) => sole
    case more      => aptus.unsupportedOperation("limited support for union types (see t210125111338)") }

  // ===========================================================================
  def isType[T: WTT]: Boolean = isType(TypeNode.parse[T])

    def isType(node: TypeNode): Boolean =
      node
        .assert(!_.isContainedBObj) // has already been validated by here (see 201014103336)
        .forceNonBObjOfni
        .pipe(_ == this)

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneString  : Boolean = isRequired && areAllSingle && isString
  def isOneInt     : Boolean = isRequired && areAllSingle && isInt
  def isOneDouble  : Boolean = isRequired && areAllSingle && isDouble
  def isOneBoolean : Boolean = isRequired && areAllSingle && isBoolean

  // ---------------------------------------------------------------------------
  def isRequired: Boolean = !_ofni.optional
  def isOptional: Boolean =  _ofni.optional

  // ---------------------------------------------------------------------------
  def isOne: Boolean = isRequired && areAllSingle
  def isOpt: Boolean = isOptional && areAllSingle
  def isNes: Boolean = isRequired && areAllMultiple
  def isPes: Boolean = isOptional && areAllMultiple
}

// ===========================================================================
