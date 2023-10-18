package gallia
package reflect
package lowlevel

// ===========================================================================
private object TypeLeafParser {

  def parseTypeNode[A: WTT]: TypeNode = _parseTypeNode(runiverse.weakTypeTag[A].tpe)

  // ---------------------------------------------------------------------------
  def _parseTypeNode(tpe: UType): TypeNode =
    TypeNode(
        leaf = apply(tpe),
        args = tpe.typeArgs.map(_parseTypeNode))

  // ===========================================================================
  private def apply(tpe: UType): TypeLeaf = {
    val symbol = tpe.typeSymbol

    val fullName: FullName = symbol.fullName.pipe(FullName.from)

    val inScopeName: String = symbol.name.encodedName.toString

    val alias: Option[Alias] = tpe.toString.pipe(fullName.alias) // eg "String" instead of "java.lang.String", but None for "foo.bar.Baz"

    val baseClassNames: List[FullNameString] = tpe.baseClasses.map(_.fullName)

    val enumValue: Boolean = fullName.isEnumValue

    // ---------------------------------------------------------------------------
    val enumeratumValueNamesOpt =
      if (FullName.containsEnumEntry(baseClassNames)) Some(ReflectUtils.enumValueNames(tpe))
      else                                            None

    // ---------------------------------------------------------------------------
    val caseClass: Boolean =
      symbol.isClass &&
      symbol.asClass.isCaseClass

    // ---------------------------------------------------------------------------
    val dataClass: Boolean =
      /**/  caseClass &&
      /**/ !enumValue &&
      /**/  enumeratumValueNamesOpt.isEmpty &&
      /**/ !fullName.startsWithScalaPackage

    // ---------------------------------------------------------------------------
    TypeLeaf(
      name        = fullName.format,
      inScopeName = inScopeName,
      alias       = alias,

      dataClass   = dataClass,
      enm         = enumValue,
      bytes       = fullName.isByteBuffer,
      inheritsSeq = FullName.containsSeq(baseClassNames),

      enumeratumValueNamesOpt = enumeratumValueNamesOpt,

      fields =
        if (dataClass) parseFields(tpe) // may in theory be empty
        else           Nil)
  }

  // ===========================================================================
  private def parseFields(tpe: UType): Seq[Field] =
    ReflectUtils
      .parseFields(tpe)
      .map { case (name, returnTpe) =>
        Field(name, _parseTypeNode(returnTpe)) }

}


// ===========================================================================
