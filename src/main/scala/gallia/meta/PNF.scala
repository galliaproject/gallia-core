package gallia
package meta

import gallia.KPath

// ===========================================================================
/** PNF = Potentially Nested Field */
// see t210125111338 (union types) - adapt
case class PNF(path: KPath, optional: Optional, multiple: Multiple, containee: Containee) extends HasKey with HasSingleContainee {
  override val key = path.key

  // ---------------------------------------------------------------------------
  @PartialTypeMatching
  def isOneBoolean: Boolean = isOne && isBoolean
  def isOneInt    : Boolean = isOne && isInt
  def isOneDouble : Boolean = isOne && isDouble
  def isOneString : Boolean = isOne && isString

  // ---------------------------------------------------------------------------
  def isOptional: Boolean =  optional
  def isRequired: Boolean = !optional
  def isMultiple: Boolean =  multiple
  def isSingle: Boolean   = !multiple

  // ---------------------------------------------------------------------------
  def isOne: Boolean = !optional && !multiple
  def isOpt: Boolean =  optional && !multiple
  def isNes: Boolean = !optional &&  multiple
  def isPes: Boolean =  optional &&  multiple
}

// ===========================================================================