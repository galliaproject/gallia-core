package gallia
package data
package json

import aptus._
import enumeratum.{Enum, EnumEntry}
import reflect.BasicType
import meta._

// ===========================================================================
/** due to the very limited set of primitives datatypes supported by JSON */
object JsonTax1 { // 220406110635
  import reflect.BasicType._

  // ---------------------------------------------------------------------------
  private val JsonTypes       : Set[BasicType] = Set(_String, _Boolean, _Double)
  private val JsonTypesWithInt: Set[BasicType] = JsonTypes + _Int

  // ---------------------------------------------------------------------------
  // because of 201119115427; this is a high price to pay to support integers with JSON...
  private val JsonIntegerTax = JsonTax1[Double](_.hasInt, d => {
    d.toInt.assert(_.toDouble == d) })

  private val JsonIntegerTax2 = JsonTax2[Double]((x: Key, y: Info) => y.isInt, d => {
    d.toInt.assert(_.toDouble == d) })

  // ===========================================================================
  def payUp(c: Cls, z: Objs): Objs = {
    val types = c.basicTypeSet

         if (types.diff(JsonTypes)       .isEmpty) z
    else if (types.diff(JsonTypesWithInt).isEmpty) z.map(JsonIntegerTax.payUp(c))
    else                                           z.map(wholeShebang(c)) // TODO: t220405114144 - optimize
  }

  // ---------------------------------------------------------------------------
  def payUp(c: Cls)(o: Obj): Obj = {
    val types = c.basicTypeSet

if (!c.hasUnions) {

         if (types.diff(JsonTypes)       .isEmpty) o
    else if (types.diff(JsonTypesWithInt).isEmpty) o.pipe(JsonIntegerTax.payUp(c))
    else                                           o.pipe(wholeShebang(c)) // TODO: t220405114144 - optimize
} else  {
  o.pipe(JsonIntegerTax2.payUp(c))
}
  }

  // ===========================================================================
  /** probably shouldn't be using JSON at this point... */
  private def wholeShebang(c: Cls)(o: Obj): Obj = // TODO: t220405114144 - optimize
    (JsonIntegerTax +: ExtendedJsonTax.values.map(_.value))
      .foldLeft(o) { (curr, tax) =>
        tax.payUp(c)(curr) }

}

// ===========================================================================
trait JsonTax0[T] {
  val transform: T   => Any
  def payUp(c: Cls)(o: Obj): Obj

  // ===========================================================================
  protected def transformation(multiple: Boolean, ncOpt: Option[Cls]): AnyValue => AnyValue =
    (multiple, ncOpt) match {
      case (false, None)     => _.asInstanceOf[    T ]     .pipe(transform)
      case (true , None)     => _.asInstanceOf[Seq[T]]     .map (transform)
      case (false, Some(nc)) => _.asInstanceOf[    Obj    ].pipe(payUp(nc))
      case (true , Some(nc)) => _.asInstanceOf[Seq[Obj   ]].map (payUp(nc)) }
}

// ===========================================================================
case class JsonTax1[T](
                qualifies: Fld => Boolean,
                transform: T   => Any)
              extends atoms.utils.ModifyObj with JsonTax0[T] {

            // ---------------------------------------------------------------------------
            final def payUp(c: Cls)(o: Obj): Obj = super.modify(c)(o)

            // ---------------------------------------------------------------------------
            final override protected def transformation(qualifyingField: Fld): AnyValue => AnyValue =
              transformation(qualifyingField.isMultiple, qualifyingField.nestedClassOpt)

            // ---------------------------------------------------------------------------
            @NumberAbstraction
            final override protected def qualifyingFields(c: Cls): Seq[Fld] =
              c
                .fields
                .filter { field =>
//!field.isString && !field.isBoolean && // optimization
                  ( qualifies(field) ||
                    field.nestedClassOpt.exists { qualifyingFields(_).nonEmpty }) }
          }

          // ===========================================================================
          case class JsonTax2[T: WTT](
                                 qualifies: (Key, Info) => Boolean,
                                 transform: T   => Any)
            extends atoms.utils.ModifyObj2[T] with JsonTax0[T] {

            // ---------------------------------------------------------------------------
            final def payUp(c: Cls)(o: Obj): Obj = super.modify(c)(o)

            // ---------------------------------------------------------------------------
            final override protected def transformation(qualifyingField: Info): AnyValue => AnyValue =
              transformation(qualifyingField.isMultiple, qualifyingField.nestingTypeOpt)

            // ---------------------------------------------------------------------------
            @NumberAbstraction
            final override protected def qualifyingFields(c: Cls): Seq[(Key, Info)] =
              c
                .fields
                .flatMap { field =>
                  val key = field.key

                  field
                    .infos
                    .flatMap { info =>
  //!field.isString && !field.isBoolean && // optimization
                      if (qualifies(key, info) ||
                          info.nestingTypeOpt.exists { qualifyingFields(_).nonEmpty })
                        Some(key, info)
                      else
                        None
                  } }
          }

// ===========================================================================
sealed trait ExtendedJsonTax extends EnumEntry {
    val value: JsonTax1[_]
  }

  // ===========================================================================
  object ExtendedJsonTax extends Enum[ExtendedJsonTax] {  
    val values = findValues

    // ---------------------------------------------------------------------------
    case object JsonByte           extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Double](_.hasByte , d => d.toByte                         .assert(_.toDouble == d)) }
    case object JsonShort          extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Double](_.isShort, d => d.toShort                        .assert(_.toDouble == d)) }
    case object JsonLong           extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Double](_.isLong , d => d.assert(doubleFitsLong) .toLong .assert(_.toDouble == d)) }
    case object JsonFloat          extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Double](_.isFloat, _     .assert(doubleFitsFloat).toFloat) } // note: precision may also be affected

    case object JsonBigInt         extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Any](_.isBigInt, stringOrLong  (BigInt    .apply, BigInt    .apply)) }
    case object JsonBigDec         extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Any](_.isBigDec, stringOrDouble(BigDecimal.apply, BigDecimal.apply)) }

    case object JsonLocalDate      extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Any](_.isLocalDate,     stringOrLong(_                  .parseLocalDate    , _.toLocalDate    ) /* aptus' */) }
    case object JsonLocalDateTime  extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Any](_.isLocalDateTime, stringOrLong(_.replace(" ", "T").parseLocalDateTime, _.toLocalDateTime) /* aptus' */) } // see https://stackoverflow.com/questions/9531524/in-an-iso-8601-date-is-the-t-character-mandatory
    case object JsonInstant        extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[Any](_.isInstant,       stringOrLong(_                  .parseInstant      , _.toInstant      ) /* aptus' */) }

    case object JsonLocalTime      extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[String](_.isLocalTime,      _.parseLocalTime      /* aptus' */) }
    case object JsonOffsetDateTime extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[String](_.isOffsetDateTime, _.parseOffsetDateTime /* aptus' */) }
    case object JsonZonedDateTime  extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[String](_.isZonedDateTime,  _.parseZonedDateTime  /* aptus' */) }

    case object JsonBinary         extends ExtendedJsonTax { lazy val value = ??? }//JsonTax[String](_.isBinary,  DataFormatting.parseBinaryString) }

    // ===========================================================================
    private def stringOrLong[T](ifString: String => T, ifLong: Long => T): Any => T =
        _ match {
          case s: String => ifString(s)
          case n: Number => ifLong  (numberToLong(n)) }
  
      // ---------------------------------------------------------------------------
      private def stringOrDouble[T](ifString: String => T, ifDouble: Double => T): Any => T =
        _ match {
          case s: String => ifString(s)
          case n: Number => ifDouble(n.doubleValue) }      
    
    // ===========================================================================
    private def numberToLong(n: Number): Long =
      n
        .doubleValue
        .assert(doubleFitsLong)
        .pipe(d => d.toLong.assert(_.toDouble == d))

    // ---------------------------------------------------------------------------
    private def doubleFitsFloat(d: Double): Boolean = 
      d <= java.lang.Float.MAX_VALUE && 
      d >= java.lang.Float.MIN_VALUE

    // ---------------------------------------------------------------------------
    private def doubleFitsLong(d: Double): Boolean = 
      d <= java.lang.Long.MAX_VALUE && 
      d >= java.lang.Long.MIN_VALUE      
  }

// ===========================================================================
