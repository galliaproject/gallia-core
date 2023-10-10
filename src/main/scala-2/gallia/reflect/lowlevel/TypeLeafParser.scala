package gallia
package reflect
package lowlevel

import aptus.Anything_

// ===========================================================================
private object TypeLeafParser {

  def parseTypeNode[A: WTT]: TypeNode = parseNode(scala.reflect.runtime.universe.weakTypeTag[A].tpe)

  // ---------------------------------------------------------------------------
  private[reflect] def parseNode(tpe: UType): TypeNode =
    TypeNode(
        leaf = TypeLeafParser(tpe),
        args = tpe.typeArgs.map(parseNode))

  // ===========================================================================
  def apply(tpe: UType): TypeLeaf = {
    val symbol = tpe.typeSymbol

    val fullName  = symbol.fullName

    val alias: Option[String] =
      this
        .alias(tpe)
        .in.noneIf(_ == fullName) // eg "String" instead of "java.lang.String", but None for "foo.bar.Baz"

    val baseClassNames: List[String] =
      tpe
        .baseClasses
        .map(_.fullName)

    // ---------------------------------------------------------------------------
    val enm: Boolean = fullName == _EnumValue

    // ---------------------------------------------------------------------------
    val enumeratumValueNamesOpt =
      if (baseClassNames.contains(_EnumEntry)) Some(ReflectUtils.enumValueNames(tpe))
      else                                     None

    // ---------------------------------------------------------------------------
    val caseClass: Boolean =
      symbol.isClass &&
      symbol.asClass.isCaseClass

    // ---------------------------------------------------------------------------
    val dataClass =
      /**/  caseClass &&
      /**/ !enm &&
      /**/  enumeratumValueNamesOpt.isEmpty &&
      /**/ !fullName.startsWith("scala.")

    // ---------------------------------------------------------------------------
    TypeLeaf(
      name        = fullName,
      inScopeName = symbol.name.encodedName.toString,      
      alias       = alias,

      dataClass   = dataClass,
      enm         = enm,
      bytes       = fullName == _ByteBuffer,      
      inheritsSeq = baseClassNames.contains(_Seq),

      enumeratumValueNamesOpt = enumeratumValueNamesOpt,

      fields      =
        if (caseClass) parseFields(tpe) // may in theory be empty
        else           Nil)
  }

  // ===========================================================================
  /** eg "Option" from "scala.Option[String]", or "String" from "java.lang.String" */
  private def alias(tpe: UType): Alias =
    tpe
      .toString
      .takeWhile(_ != '[') /* TODO: cleaner way? */
      .pipe(simplifyFullName)

  // ---------------------------------------------------------------------------
  private def parseFields(tpe: UType): Seq[Field] =
    ReflectUtils
      .parseFields(tpe)
      .map { case (name, returnTpe) =>
        Field(name, parseNode(returnTpe)) }

}


// ===========================================================================
