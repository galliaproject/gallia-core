package gallia.reflect

import aptus.Anything_
import aptus.Name

// ===========================================================================
private object ReflectUtils {

  /** eg "Option" from "scala.Option[String]", or "String" from "java.lang.String" */
  def alias(tpe: UType) =
    tpe
      .toString
      .takeWhile(_ != '[') /* TODO: cleaner way? */
      .thn(simplify)

  // ---------------------------------------------------------------------------
  def simplify(value: FullName): Alias =
    value
      .stripPrefix("java.lang.")
      .stripPrefix("java.time.")
      .stripPrefix("java.math.")
      .stripPrefix("scala.package.")
      .stripPrefix("scala.")
      .stripPrefix("enumeratum.")

  // ===========================================================================
  def parseFields(tpe: UType): List[(Name, UType)] =
    tpe
      .decls
      .filter((x: scala.reflect.api.Symbols#SymbolApi) => x.isMethod)
      .map   (_.asMethod)
      .filter(_.isCaseAccessor)
      .toList
      .map { method =>
        val name = method.name.decodedName.toString
        val tpe2 = method.typeSignature.resultType

        (name, tpe2) }
}

// ===========================================================================
