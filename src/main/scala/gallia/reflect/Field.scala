package gallia
package reflect

import aptus.String_

// ===========================================================================
case class Field(key: String, node: TypeNode) {
    override def toString: String = formatDefault
      def formatDefault: String = s"${key.escapeQuotes.quote}\t${node.formatDefault}"

    // ---------------------------------------------------------------------------
    def unaliased: Field = Field(key, node.unaliased)
  }

  // ===========================================================================
  object Field {
    def parseAll[A: WTT]: Any = parseAll(scala.reflect.runtime.universe.weakTypeTag[A].tpe)

    // ---------------------------------------------------------------------------
    def parseAll(tpe: UType): Seq[Field] =
      ReflectUtils
        .parseFields(tpe)
        .map { case (name, returnTpe) =>
          Field(name, TypeNode.parse(returnTpe)) }

  }

// ===========================================================================
