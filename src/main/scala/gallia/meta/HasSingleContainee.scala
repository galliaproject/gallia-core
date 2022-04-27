package gallia.meta

// ===========================================================================
trait HasSingleContainee {
  protected val containee: Containee

  // ---------------------------------------------------------------------------
  def      nestedClassOpt: Option[Cls] = containee.nestingOpt
  def forceNestedClass   :        Cls  = nestedClassOpt.get

  // ---------------------------------------------------------------------------
  def isNestingWithName(name: String): Boolean = containee.nestingOpt.exists(_.nameOpt == Some(name))

  def isNesting  : Boolean = containee.nestingOpt.nonEmpty
  def isBasicType: Boolean = containee.leafOpt   .nonEmpty

  // ---------------------------------------------------------------------------
  def isBasicType(value: BasicType): Boolean = containee.leafOpt.exists(_ == value)

  // ---------------------------------------------------------------------------
  def isBoolean : Boolean = isBasicType(BasicType._Boolean)
  def isString  : Boolean = isBasicType(BasicType._String)
  def isInt     : Boolean = isBasicType(BasicType._Int)
  def isDouble  : Boolean = isBasicType(BasicType._Double)
}

// ===========================================================================
