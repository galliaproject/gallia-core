package gallia.reflect

import aptus.Anything_

// ===========================================================================
private object TypeLeafParser {

  def apply(tpe: UType): TypeLeaf = {
    val symbol = tpe.typeSymbol

    val name  = symbol.fullName

    val alias: Option[String] =
      ReflectUtils
        .alias(tpe)
        .as.noneIf(_ == name) // eg "String" instead of "java.lang.String", but None for "foo.bar.Baz"

    val isCaseClass: Boolean =
      symbol.isClass &&
      symbol.asClass.isCaseClass

    val baseClassNames: List[String] =
      tpe
        .baseClasses
        .map(_.fullName)

    // ---------------------------------------------------------------------------
    TypeLeaf(
      name,
      alias,
      dataClass   = isCaseClass && !name.startsWith("scala."),
      enum        = baseClassNames.contains(_EnumEntry),
      inheritsSeq = baseClassNames.contains(_Seq),
      fields      =
        if (isCaseClass) Field.parseAll(tpe) // may in theory be empty
        else             Nil)
  }

}


// ===========================================================================
