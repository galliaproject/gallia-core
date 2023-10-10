package gallia
package reflect
package lowlevel

// ===========================================================================
object ConcreteReflectionUtils extends ReflectionUtilsAbstraction {
  def fullName[T : WTT]: FullName = ReflectUtils.fullName[T]
  def ctag    [T : WTT]: CT[T]    = ReflectUtils.ctag[T]
  def typeNode[T : WTT]: TypeNode = TypeLeafParser.parseTypeNode[T]

  // ===========================================================================
  def instantiateFromFirstTypeArgFirstTypeArg[T: WTT]: target.Instantiator = InstantiatorUtils.fromFirstTypeArgFirstTypeArg[T]
  def instantiateFromFirstTypeArg            [T: WTT]: target.Instantiator = InstantiatorUtils.fromFirstTypeArg[T]
  def instantiateFromTypeDirectly            [T: WTT]: target.Instantiator = InstantiatorUtils.fromTypeDirectly[T]

  // ===========================================================================
  /** enum must not be nested somehow */
  def enumValueNames[$EnumEntry <: EnumEntry : WTT]                            : Seq[EntryNameString] = ReflectUtils.enumValueNames[$EnumEntry]
  def withEntryName [$EnumEntry <: EnumEntry : WTT](entryName: EntryNameString): $EnumEntry           = ReflectUtils.withEntryName [$EnumEntry](entryName) }

// ===========================================================================
