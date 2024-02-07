package gallia
package reflect
package macros3

import scala.quoted.{Quotes, Expr, ToExpr}

// ===========================================================================
object TypeNodeToExpr {

  given ToExpr[Field] = new ToExpr[Field] {
    def apply(x: Field)(using Quotes) =
      fieldToExpr(x) }

  // ---------------------------------------------------------------------------
  given ToExpr[TypeLeaf] = new ToExpr[TypeLeaf] {
    def apply(x: TypeLeaf)(using Quotes) =
      typeLeafToExpr(x) }

  // ---------------------------------------------------------------------------
  given ToExpr[TypeNode] = new ToExpr[TypeNode] {
    def apply(x: TypeNode)(using Quotes) =
      typeNodeToExpr(x) }

  // ===========================================================================
  def typeNodeToExpr(x: TypeNode)(using Quotes) =
      '{TypeNode(
          leaf = ${Expr(x.leaf)},
          args = ${Expr(x.args)}) }

    // ---------------------------------------------------------------------------
    private def typeLeafToExpr(x: TypeLeaf)(using Quotes) =
      '{TypeLeaf(
          name        = ${Expr(x.name)},

          dataClass       = ${Expr(x.dataClass)},
          galliaEnumValue = ${Expr(x.galliaEnumValue)},
          bytes           = ${Expr(x.bytes)},
          inheritsSeq     = ${Expr(x.inheritsSeq)},

          enumeratumValueNamesOpt = ${Expr(x.enumeratumValueNamesOpt)},
          fields = ${Expr(x.fields)}) }

    // ---------------------------------------------------------------------------
    private def fieldToExpr(x: Field)(using Quotes) =
      '{Field(
        key      = ${Expr(x.key)},
        typeNode = ${Expr(x.typeNode)})} }

// ===========================================================================
