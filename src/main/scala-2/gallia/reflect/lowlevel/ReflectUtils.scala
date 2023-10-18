package gallia
package reflect
package lowlevel

import scala.reflect.api

// ===========================================================================
private object ReflectUtils {

  private[reflect] def parseFields(tpe: UType): List[(String, UType)] =
      _methodSymbols(tpe)
        .map { method =>
          val name = method.name.decodedName.toString
          val tpe2 = method.typeSignature.resultType

          (name, tpe2) }

    // ---------------------------------------------------------------------------
    private def _methodSymbols(tpe: UType) =
      tpe
        .decls
        .filter((x: api.Symbols#SymbolApi) => x.isMethod)
        .map   (_.asMethod)
        .filter(_.isCaseAccessor)
        .toList

    // ---------------------------------------------------------------------------
    private[reflect] def methodSymbols(tpe: scala.reflect.runtime.universe.Type) = // can't easily refactor with above, so at least keep them together
      tpe
        .decls
        .filter((x: api.Symbols#SymbolApi) => x.isMethod)
        .map   (_.asMethod)
        .filter(_.isCaseAccessor)
        .toList

  // ===========================================================================
  /** enum must not be nested somehow */
  private[reflect] def enumValueNames(tpe: UType): Seq[String] =
    tpe
      .companion
      .members
      .filter { (symbol: api.Universe#Symbol) =>
        symbol.isPublic && symbol.isStatic && symbol.isModule }
      .map { (symbol: api.Universe#Symbol) =>
        symbol.name.decodedName.toString }
      .toList
.reverse /* TODO: always? */

}

// ===========================================================================
