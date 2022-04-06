package gallia
package data.json

import aptus._
import enumeratum.{Enum, EnumEntry}
import meta._
import reflect.BasicType

// ===========================================================================
/** due to the very limited set of primitives datatypes supported by JSON */
object JsonTax { import reflect.BasicType._  
  private val JsonTypes       : Set[BasicType] = Set(_String, _Boolean, _Double)
  private val JsonTypesWithInt: Set[BasicType] = JsonTypes + _Int

  // ---------------------------------------------------------------------------
  private val JsonIntegerTax = JsonTax[Double](_.isInt, _.toInt)// because of 201119115427; this is a high price to pay to support integers...
  
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

         if (types.diff(JsonTypes)       .isEmpty) o
    else if (types.diff(JsonTypesWithInt).isEmpty) o.pipe(JsonIntegerTax.payUp(c))
    else                                           o.pipe(wholeShebang(c)) // TODO: t220405114144 - optimize
  }  

  // ===========================================================================
  /** probably shouldn't be using JSON at this point... */
  private def wholeShebang(c: Cls)(o: Obj): Obj = // TODO: t220405114144 - optimize
    (JsonIntegerTax +: ExtendedJsonTax.values.map(_.value))
      .foldLeft(o) { (curr, tax) =>
        tax.payUp(c)(curr) }      

}

// ===========================================================================
case class JsonTax[T](
      qualifies: Fld => Boolean,
      transform: T   => Any) 
    extends atoms.utils.ModifyObj {

  // ---------------------------------------------------------------------------
  final def payUp(c: Cls)(o: Obj): Obj = super.modify(c)(o)

  // ===========================================================================
  @NumberAbstraction
  final override protected def qualifyingFields(c: Cls): Seq[Fld] =
    c
      .fields
      .filter { field =>
        !field.isString && !field.isBoolean && // optimization
        ( qualifies(field) || 
          field.nestedClassOpt.exists { qualifyingFields(_).nonEmpty }) }

  // ===========================================================================
  final override protected def transformation(qualifyingField: Fld): AnyValue => AnyValue =
    (qualifyingField.isMultiple, qualifyingField.nestedClassOpt) match {
      case (false, None)     => _.asInstanceOf[    T ]     .pipe(transform)
      case (true , None)     => _.asInstanceOf[Seq[T]]     .map (transform)
      case (false, Some(c2)) => _.asInstanceOf[    Obj    ].pipe(payUp(c2))
      case (true , Some(c2)) => _.asInstanceOf[Seq[Obj   ]].map (payUp(c2)) }

}

// ===========================================================================
sealed trait ExtendedJsonTax extends EnumEntry {
    val value: JsonTax[_]
  }

  // ===========================================================================
  object ExtendedJsonTax extends Enum[ExtendedJsonTax] {  
    val values = findValues

    // ---------------------------------------------------------------------------
    case object JsonByte           extends ExtendedJsonTax { val value = JsonTax[Double](_.isByte , _.toByte) }    
    case object JsonShort          extends ExtendedJsonTax { val value = JsonTax[Double](_.isShort, _.toShort) }
    case object JsonLong           extends ExtendedJsonTax { val value = JsonTax[Double](_.isLong , _.toLong) }
    case object JsonFloat          extends ExtendedJsonTax { val value = JsonTax[Double](_.isFloat, _.toFloat) }
    
    case object JsonBigInt         extends ExtendedJsonTax { val value = JsonTax[String](_.isBigInt, BigInt    .apply) }
    case object JsonBigDec         extends ExtendedJsonTax { val value = JsonTax[String](_.isBigDec, BigDecimal.apply) }
    
    case object JsonLocalDate      extends ExtendedJsonTax { val value = JsonTax[Any](_.isLocalDate,      mult(_                  .parseLocalDate    , _.toLocalDate    ) /* aptus' */) }
    case object JsonLocalDateTime  extends ExtendedJsonTax { val value = JsonTax[Any](_.isLocalDateTime,  mult(_.replace(" ", "T").parseLocalDateTime, _.toLocalDateTime) /* aptus' */) } // see https://stackoverflow.com/questions/9531524/in-an-iso-8601-date-is-the-t-character-mandatory
    case object JsonInstant        extends ExtendedJsonTax { val value = JsonTax[Any](_.isInstant,        mult(_                  .parseInstant      , _.toInstant      ) /* aptus' */) }
    
    case object JsonLocalTime      extends ExtendedJsonTax { val value = JsonTax[String](_.isLocalTime,      _.parseLocalTime      /* aptus' */) }
    case object JsonOffsetDateTime extends ExtendedJsonTax { val value = JsonTax[String](_.isOffsetDateTime, _.parseOffsetDateTime /* aptus' */) }
    case object JsonZonedDateTime  extends ExtendedJsonTax { val value = JsonTax[String](_.isZonedDateTime,  _.parseZonedDateTime  /* aptus' */) }
    
    // ===========================================================================
    private def mult[T](ifString: String => T, ifLong: Long => T): Any => T =
      _ match {
        case s: String => ifString(s)
        case n: Number => ifLong  (n.longValue) }     
  }

// ===========================================================================
