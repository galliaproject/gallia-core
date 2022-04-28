package gallia
package meta

// ===========================================================================
trait HasOptionalAndMultiple {
  def optional : Optional
  def multiple : Multiple

  // ===========================================================================
  def isRequired: Boolean = !optional
  def isOptional: Boolean =  optional

  // ---------------------------------------------------------------------------
  def isMultiple: Boolean =  multiple
  def isSingle  : Boolean = !multiple

  // ---------------------------------------------------------------------------
  def isOne: Boolean = isRequired && isSingle
  def isOpt: Boolean = isOptional && isSingle
  def isNes: Boolean = isRequired && isMultiple
  def isPes: Boolean = isOptional && isMultiple

  // ---------------------------------------------------------------------------
  @deprecated def container: Container = Container.from(optional, multiple)
}

// ===========================================================================
