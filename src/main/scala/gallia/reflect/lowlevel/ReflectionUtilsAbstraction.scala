package gallia
package reflect
package lowlevel

// ===========================================================================
/** differs based on 2.x vs 3.x */
trait ReflectionUtilsAbstraction {
  def fullName[T : WTT]: FullName
  def ctag    [T : WTT]: CT[T]
  def typeNode[T : WTT]: TypeNode

  // ---------------------------------------------------------------------------
  def instantiateFromFirstTypeArgFirstTypeArg[T: WTT]: target.Instantiator
  def instantiateFromFirstTypeArg            [T: WTT]: target.Instantiator
  def instantiateFromTypeDirectly            [T: WTT]: target.Instantiator

  // ---------------------------------------------------------------------------
  def withEntryName[$EnumEntry <: EnumEntry: WTT](entryName: String): $EnumEntry

  /** enum must not be nested somehow */
  def enumValueNames[$EnumEntry <: EnumEntry : WTT]: Seq[String] }

// ===========================================================================
