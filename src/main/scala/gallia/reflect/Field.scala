package gallia
package reflect

import aptus.String_

// ===========================================================================
case class Field(key: String, node: TypeNode) {

  override def toString: String = formatDefault
    def formatDefault: String = s"${key.escapeQuotes.quote}\t${node.formatDefault}"

  // ---------------------------------------------------------------------------
  def unaliased: Field = Field(key, node.unaliased) }

// ===========================================================================
