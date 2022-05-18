package gallia
package meta

// ===========================================================================
trait HasSingleValueType {
  protected val valueType: ValueType

  // ---------------------------------------------------------------------------
  def      nestedClassOpt: Option[Cls] = valueType.nestingOpt
  def forceNestedClass   :        Cls  = nestedClassOpt.get

  // ---------------------------------------------------------------------------
  def isNestingWithName(name: String): Boolean = valueType.nestingOpt.exists(_.nameOpt == Some(name))

  def isNesting  : Boolean = valueType.nestingOpt.nonEmpty
  def isBasicType: Boolean = valueType.leafOpt   .nonEmpty

  // ---------------------------------------------------------------------------
  def isBasicType(value: BasicType)          : Boolean = valueType.leafOpt.exists(_ == value)
  def isBasicType(pred: BasicType => Boolean): Boolean = valueType.isBasicType(pred)

  // ---------------------------------------------------------------------------
  def isBoolean : Boolean = isBasicType(BasicType._Boolean)
  def isString  : Boolean = isBasicType(BasicType._String)
  def isInt     : Boolean = isBasicType(BasicType._Int)
  def isDouble  : Boolean = isBasicType(BasicType._Double)

  def isEnm     : Boolean = isBasicType(_.isEnm)
}

// ===========================================================================
