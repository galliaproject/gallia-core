package gallia
package data
package json

import aptus._
import meta._
import gallia.atoms.utils._

// ===========================================================================
sealed trait JsonTax extends EnumEntry {
    def valueOpt(c: Cls): Option[Obj => Obj] }

  // ===========================================================================
  /** due to the very limited set of primitives datatypes supported by JSON */
  object JsonTax extends Enum[JsonTax] { // 220406110635
    val values = findValues

    // ===========================================================================
    def payUp(c: Cls)(o: Obj): Obj  = taxesOpt(c) match {
        case None        => o
        case Some(taxes) => taxes(o) }

    // ---------------------------------------------------------------------------
    def payUp(c: Cls, z: Objs): Objs = taxesOpt(c) match {
        case None        => z
        case Some(taxes) => z.map(taxes) }

    // ===========================================================================
    private def taxesOpt(c: Cls): Option[Obj => Obj] = // TODO: t220405114144 - optimize
        values
          .flatMap  (_.valueOpt(c))
          .in.noneIf(_.isEmpty)
          .map { taxes => taxes.foldLeft(_) { (curr, tax) => tax(curr) } }

      // ===========================================================================
      import JsonTaxUtils._

      case object JsonIntTax            extends JsonTax { def valueOpt(c: Cls) = jsonTax[Double](c)(_.hasInt)  (d => d.toInt.assert(_.toDouble == d)) }

      case object JsonByteTax           extends JsonTax { def valueOpt(c: Cls) = jsonTax[Double](c)(_.hasByte) (d => d.toByte                         .assert(_.toDouble == d)) }
      case object JsonShortTax          extends JsonTax { def valueOpt(c: Cls) = jsonTax[Double](c)(_.hasShort)(d => d.toShort                        .assert(_.toDouble == d)) }
      case object JsonLongTax           extends JsonTax { def valueOpt(c: Cls) = jsonTax[Double](c)(_.hasLong) (d => d.assert(doubleFitsLong) .toLong .assert(_.toDouble == d)) }
      case object JsonFloatTax          extends JsonTax { def valueOpt(c: Cls) = jsonTax[Double](c)(_.hasFloat)(_     .assert(doubleFitsFloat).toFloat) } // note: precision may also be affected

      case object JsonBigIntTax         extends JsonTax { def valueOpt(c: Cls) = jsonTax[Any]   (c)(_.hasBigInt)(stringOrLong  (BigInt    .apply, BigInt    .apply)) }
      case object JsonBigDecTax         extends JsonTax { def valueOpt(c: Cls) = jsonTax[Any]   (c)(_.hasBigDec)(stringOrDouble(BigDecimal.apply, BigDecimal.apply)) }

      case object JsonLocalDateTax      extends JsonTax { def valueOpt(c: Cls) = jsonTax[Any]   (c)(_.hasLocalDate)    (stringOrLong(_                  .parseLocalDate    , _.toLocalDate    ) /* aptus' */) }
      case object JsonLocalDateTimeTax  extends JsonTax { def valueOpt(c: Cls) = jsonTax[Any]   (c)(_.hasLocalDateTime)(stringOrLong(_.replace(" ", "T").parseLocalDateTime, _.toLocalDateTime) /* aptus' */) } // see https://stackoverflow.com/questions/9531524/in-an-iso-8601-date-is-the-t-character-mandatory
      case object JsonInstantTax        extends JsonTax { def valueOpt(c: Cls) = jsonTax[Any]   (c)(_.hasInstant)      (stringOrLong(_                  .parseInstant      , _.toInstant      ) /* aptus' */) }

      case object JsonLocalTimeTax      extends JsonTax { def valueOpt(c: Cls) = jsonTax[String](c)(_.hasLocalTime)     (_.parseLocalTime      /* aptus' */) }
      case object JsonOffsetDateTimeTax extends JsonTax { def valueOpt(c: Cls) = jsonTax[String](c)(_.hasOffsetDateTime)(_.parseOffsetDateTime /* aptus' */) }
      case object JsonZonedDateTimeTax  extends JsonTax { def valueOpt(c: Cls) = jsonTax[String](c)(_.hasZonedDateTime) (_.parseZonedDateTime  /* aptus' */) }

      case object JsonBinaryTax         extends JsonTax { def valueOpt(c: Cls) = jsonTax[String](c)(_.hasBinary)(DataFormatting.parseBinaryString) }

      // ===========================================================================
      private def jsonTax[T: WTT](c: Cls)(pred: Fld => Boolean)(f: T => Any): Option[Obj => Obj] =
        ObjModifierCtx
          .parse(c)(pred)
          .map(new ObjModifier[T](c.hasUnions, _, f))
          .map(_.modify)
  }

// ===========================================================================
