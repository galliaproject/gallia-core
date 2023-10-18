package gallia
package reflect
package lowlevel

// ===========================================================================
object ConcreteReflectionUtils extends ReflectionUtilsAbstraction {
  def fullName[T : WTT]: FullNameString = runiverse.weakTypeTag[T].tpe.typeSymbol.fullName
  def typeNode[T : WTT]: TypeNode       = TypeLeafParser.parseTypeNode[T]

  // ---------------------------------------------------------------------------
  // see https://stackoverflow.com/questions/18729321/how-to-get-classtag-form-typetag-or-both-at-same-time
  def ctag[T: WTT]: CT[T] =
    scala.reflect.ClassTag[T](
      runiverse.weakTypeTag[T].mirror.runtimeClass(
        runiverse.weakTypeTag[T].tpe))

  // ===========================================================================
  def instantiateFromFirstTypeArgFirstTypeArg[T: WTT]: target.Instantiator = InstantiatorUtils.fromFirstTypeArgFirstTypeArg[T]
  def instantiateFromFirstTypeArg            [T: WTT]: target.Instantiator = InstantiatorUtils.fromFirstTypeArg[T]
  def instantiateFromTypeDirectly            [T: WTT]: target.Instantiator = InstantiatorUtils.fromTypeDirectly[T]

  // ===========================================================================
  /** enum must not be nested somehow */
  def enumValueNames[$EnumEntry <: EnumEntry : WTT]: Seq[EntryNameString] = 
    ReflectUtils.enumValueNames(runiverse.weakTypeTag[$EnumEntry].tpe)

  // ---------------------------------------------------------------------------
  def withEntryName[$EnumEntry <: EnumEntry: WTT](entryName: EntryNameString): $EnumEntry =
    CompanionReflection[$EnumEntry](methodName = "withName")(
      /* args */ entryName) }

// ===========================================================================
