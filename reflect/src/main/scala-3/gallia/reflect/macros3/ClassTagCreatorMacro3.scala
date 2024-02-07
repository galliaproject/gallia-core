package gallia
package reflect
package macros3

import aptus.*

// ===========================================================================
private object ClassTagCreatorMacro3 {
  import scala.quoted.{Quotes, quotes, Type, Expr}

  // ---------------------------------------------------------------------------
  // TODO: t231228134643 - there must be a simpler way...
  def apply[T: Type](using q: Quotes)(tpe: q.reflect.TypeRepr): q.reflect.Term = { import q.reflect.*
    val typeTree = Inferred(tpe)

    Apply(
     TypeApply(
       Select.unique('{scala.reflect.ClassTag}.asTerm, "apply"),
       List(typeTree)),
      List(
       TypeApply(
         Select.unique('{scala.Predef}.asTerm, "classOf"),
         List(typeTree)))) } }

// ===========================================================================
