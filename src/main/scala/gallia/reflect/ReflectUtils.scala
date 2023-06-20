package gallia
package reflect

import aptus.Name
import scala.reflect.api

// ===========================================================================
object ReflectUtils {

  /** eg "Option" from "scala.Option[String]", or "String" from "java.lang.String" */
  private[reflect] def alias(tpe: UType): Alias =
    tpe
      .toString
      .takeWhile(_ != '[') /* TODO: cleaner way? */
      .pipe(simplify)

  // ---------------------------------------------------------------------------
  def fullNameFromType (tpe: UType): FullName = tpe.typeSymbol.fullName
  def fullNameFromValue(value: Any): FullName = value.getClass.getName.pipe(BasicTypeUtils.normalizeFullName) // TODO: t220411094433 - hopefully there's a cleaner way...

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
    def methodSymbols(tpe: scala.reflect.runtime.universe.Type) = // can't easily refactor with above, so at least keep them together
      tpe
        .decls
        .filter((x: api.Symbols#SymbolApi) => x.isMethod)
        .map   (_.asMethod)
        .filter(_.isCaseAccessor)
        .toList

  // ===========================================================================
  /** enum must not be nested somehow */
  def enumValueNames[T <: enumeratum.EnumEntry : WTT]: Seq[String] = enumValueNames(scala.reflect.runtime.universe.weakTypeTag[T].tpe)

  // ---------------------------------------------------------------------------
  def     name[T : WTT]: String = scala.reflect.runtime.universe.weakTypeTag[T].tpe.typeSymbol.name.decodedName.toString
  def fullName[T : WTT]: String = scala.reflect.runtime.universe.weakTypeTag[T].tpe.typeSymbol.fullName

  // ---------------------------------------------------------------------------
  /** enum must not be nested somehow */
  def enumValueNames(tpe: UType): Seq[String] =
    tpe
      .companion
      .members
      .filter { symbol: api.Universe#Symbol =>
        symbol.isPublic && symbol.isStatic && symbol.isModule }
      .map { symbol: api.Universe#Symbol =>
        symbol.name.decodedName.toString }
      .toList
.reverse /* TODO: always? */

  // ===========================================================================
  def withEntryName[T <: EnumEntry: WTT](entryName: String) =
    CompanionReflection[T](methodName = "withName")(
        /* args */ entryName)

}

// ===========================================================================
