package gallia
package reflect

// ===========================================================================
object ReflectUtils {

  private[reflect] def alias(tpe: UType): Alias = ???

  def     name[T : WTT]: String = ???
  def fullName[T : WTT]: String = ???

  // ---------------------------------------------------------------------------
  def fullNameFromType (tpe: UType): FullName = ???
  def fullNameFromValue(value: Any): FullName = ???

  // ---------------------------------------------------------------------------
  def simplify(value: FullName): Alias = ???

  // ===========================================================================
  def parseFields(tpe: UType): List[(Name, UType)] = ???

  // ---------------------------------------------------------------------------
  def methodSymbols(tpe: UType) = ???

  // ===========================================================================
  /** enum must not be nested somehow */
  def enumValueNames[T <: enumeratum.EnumEntry : WTT]: Seq[String] = ???

  // ---------------------------------------------------------------------------
  /** enum must not be nested somehow */
  def enumValueNames(tpe: UType): Seq[String] = ???

  // ===========================================================================
  def withEntryName[$EnumEntry <: EnumEntry: WTT](entryName: String): $EnumEntry = ???

  // ===========================================================================
  // see https://stackoverflow.com/questions/18729321/how-to-get-classtag-form-typetag-or-both-at-same-time
  def ctag[T: WTT]: CT[T] = ???
}

// ===========================================================================
