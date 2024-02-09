package gallia
package reflect
package lowlevel

// ===========================================================================
/*private - for macros2 */object TypeLeafParserRuntime2 extends ReflectionTypesAbstraction {
  def parseTypeNode[A: WTT]: TypeNode = _parseTypeNode(runiverse.weakTypeTag[A].tpe)

  // ---------------------------------------------------------------------------
  def _parseTypeNode(tpe: UType): TypeNode =
    TypeNode(
        leaf = apply(tpe),
        args = tpe.typeArgs.map(_parseTypeNode))

  // ===========================================================================
  private def apply(tpe: UType): TypeLeaf = {
    val symbol = tpe.typeSymbol

    val fullName       : FullyQualifiedName   = symbol.fullName.pipe(FullyQualifiedName.from)
    val baseClassNames : List[FullNameString] = tpe.baseClasses.map(_.fullName)
    val galliaEnumValue: Boolean = fullName.isGalliaEnumValue

    // ---------------------------------------------------------------------------
    val enumeratumValueNamesOpt =
      if (FullyQualifiedName.containsEnumEntry(baseClassNames)) Some(ReflectUtils.enumValueNames(tpe))
      else                                                      None

    // ---------------------------------------------------------------------------
    val caseClass: Boolean =
      symbol.isClass &&
      symbol.asClass.isCaseClass

    // ---------------------------------------------------------------------------
    val dataClass: Boolean =
      /**/  caseClass &&
      /**/ !baseClassNames.exists(_ == FullNameBuiltIns._ScalaAnyVal) &&
      /**/  enumeratumValueNamesOpt.isEmpty &&
      /**/ !fullName.startsWithScalaPackage &&
      /**/ !fullName.startsWithGalliaPackage /* exclude eg EnumValue, BObj, ... */

    // ---------------------------------------------------------------------------
    TypeLeaf(
      name        = fullName.format,

      dataClass       = dataClass,
      galliaEnumValue = galliaEnumValue,
      bytes           = fullName.isByteBuffer,
      inheritsSeq     = FullyQualifiedName.containsSeq(baseClassNames),

      enumeratumValueNamesOpt = enumeratumValueNamesOpt,

      // may in theory be empty
      fields =
        if (!dataClass) List.empty
        else
          ReflectUtils
            .parseFields(tpe)
            .map { case (name, returnTpe) =>
              Field(name, _parseTypeNode(returnTpe)) } ) } }

// ===========================================================================
