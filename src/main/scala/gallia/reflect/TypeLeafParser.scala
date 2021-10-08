package gallia.reflect

import aptus.Anything_

// ===========================================================================
private object TypeLeafParser {

  def apply(tpe: UType): TypeLeaf = {
    val symbol = tpe.typeSymbol

    val fullName  = symbol.fullName

    val alias: Option[String] =
      ReflectUtils
        .alias(tpe)
        .in.noneIf(_ == fullName) // eg "String" instead of "java.lang.String", but None for "foo.bar.Baz"

    val isCaseClass: Boolean =
      symbol.isClass &&
      symbol.asClass.isCaseClass

    val baseClassNames: List[String] =
      tpe
        .baseClasses
        .map(_.fullName)

    // ---------------------------------------------------------------------------
    TypeLeaf(
      name        = fullName,
      inScopeName = symbol.name.encodedName.toString,      
      alias       = alias,
      dataClass   = isCaseClass && !fullName.startsWith("scala."),
      enum        = baseClassNames.contains(_EnumEntry),
      inheritsSeq = baseClassNames.contains(_Seq),
      fields      =
        if (isCaseClass) Field.parseAll(tpe) // may in theory be empty
        else             Nil)
  }

}


// ===========================================================================
