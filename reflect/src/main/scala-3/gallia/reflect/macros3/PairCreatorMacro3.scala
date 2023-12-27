package gallia
package reflect
package macros3

// ===========================================================================
object PairCreatorMacro3 {
  import scala.quoted.{Quotes, quotes, Type, Expr}
  import TypeNodeToExpr._

  // ---------------------------------------------------------------------------
  def apply[T: Type](using q: Quotes): Expr[(TypeNode, Instantiator)] = {
    import quotes.reflect.*

    val tpe      = TypeRepr.of[T]
    val typeNode = TypeLeafParserMacro3.rec(using q)(tpe)

    '{ ${Expr(typeNode)} ->
       ${InstantiatorCreatorMacro3.rec(using q)(tpe)(typeNode).asExprOf[Instantiator]}} }
}

// ===========================================================================
