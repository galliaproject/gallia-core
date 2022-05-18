package gallia
package meta

import aptus._

// ===========================================================================
trait InfoLike extends SubInfosLike {
  protected val _info: Info

  // ===========================================================================
  def isType[T: WTT]: Boolean = isType(TypeNode.parse[T])

    def isType(node: TypeNode): Boolean =
      node
        .assert(!_.isContainedBObj) // has already been validated by here (see 201014103336)
        .forceNonBObjInfo
        .pipe(_ == this)

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneString  : Boolean = isRequired && areAllSingle && isString
  def isOneInt     : Boolean = isRequired && areAllSingle && isInt
  def isOneDouble  : Boolean = isRequired && areAllSingle && isDouble
  def isOneBoolean : Boolean = isRequired && areAllSingle && isBoolean

  // ---------------------------------------------------------------------------
  def isRequired: Boolean = !_info.optional
  def isOptional: Boolean =  _info.optional

  // ---------------------------------------------------------------------------
  def isOne: Boolean = isRequired && areAllSingle
  def isOpt: Boolean = isOptional && areAllSingle
  def isNes: Boolean = isRequired && areAllMultiple
  def isPes: Boolean = isOptional && areAllMultiple

  // ===========================================================================
  def trivialValueTypeOpt: Option[ValueType] =  _info.union.ifOneOpt.flatMap(_.inSomeIf(_ => isRequired)).map(_.valueType)

  // ===========================================================================
  private[gallia] def enmInfo1    (multiple: Boolean): Info1     = enmSubInfo(multiple).info1(_info.optional)
  private[gallia] def enmValueType(multiple: Boolean): ValueType = enmSubInfo(multiple).valueType
  private[gallia] def enmSubInfo  (multiple: Boolean): SubInfo   = _info.union.filter(_.isEnmMatching(multiple)).force.one
}

// ===========================================================================
