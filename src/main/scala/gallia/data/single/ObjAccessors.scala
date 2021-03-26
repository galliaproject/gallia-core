package gallia.data.single

import aptus.Anything_
import aptus.{Opt, Pes}

import gallia.data.DataFormatting.{formatBasicValue => format}

// ===========================================================================
@gallia.TypeMatching
trait ObjAccessors { _: Obj => // id210326140514
  import ObjAccessors._nmbr
  import ValueWrapper.to

  // ---------------------------------------------------------------------------
  def string  (key: KPathW):     String  = force(key)      .asString
  def string_ (key: KPathW): Opt[String] = opt  (key).map(_.asString)
  def strings (key: KPathW): Seq[String] = force(key)      .asSeq.map(_.asString)
  def strings_(key: KPathW): Pes[String] = opt  (key).map(_.asSeq.map(_.asString))

  def int  (key: KPathW):     Int  = force(key)    .asInt
  def int_ (key: KPathW): Opt[Int] = opt(key).map(_.asInt)
  def ints (key: KPathW): Seq[Int] = force(key)      .asSeq.map(_.asInt)
  def ints_(key: KPathW): Pes[Int] = opt  (key).map(_.asSeq.map(_.asInt))

  def double  (key: KPathW):     Double  = force(key)      .asDouble
  def double_ (key: KPathW): Opt[Double] = opt  (key).map(_.asDouble)
  def doubles (key: KPathW): Seq[Double] = force(key)      .asSeq.map(_.asDouble)
  def doubles_(key: KPathW): Pes[Double] = opt  (key).map(_.asSeq.map(_.asDouble))

  def boolean  (key: KPathW):     Boolean  = force(key)      .asBoolean
  def boolean_ (key: KPathW): Opt[Boolean] = opt  (key).map(_.asBoolean)
  def booleans (key: KPathW): Seq[Boolean] = force(key)      .asSeq.map(_.asBoolean)
  def booleans_(key: KPathW): Pes[Boolean] = opt  (key).map(_.asSeq.map(_.asBoolean))

  // ---------------------------------------------------------------------------
  def byte  (key: KPathW):     Byte  = force(key)      .asByte
  def byte_ (key: KPathW): Opt[Byte] = opt  (key).map(_.asByte)
  def bytes (key: KPathW): Seq[Byte] = force(key)      .asSeq.map(_.asByte)
  def bytes_(key: KPathW): Pes[Byte] = opt  (key).map(_.asSeq.map(_.asByte))

  def short  (key: KPathW):     Short  = force(key)    .asShort
  def short_ (key: KPathW): Opt[Short] = opt(key).map(_.asShort)
  def shorts (key: KPathW): Seq[Short] = force(key)      .asSeq.map(_.asShort)
  def shorts_(key: KPathW): Pes[Short] = opt  (key).map(_.asSeq.map(_.asShort))

  def long  (key: KPathW):     Long  = force(key)      .asLong
  def long_ (key: KPathW): Opt[Long] = opt  (key).map(_.asLong)
  def longs (key: KPathW): Seq[Long] = force(key)      .asSeq.map(_.asLong)
  def longs_(key: KPathW): Pes[Long] = opt  (key).map(_.asSeq.map(_.asLong))

  def float  (key: KPathW):     Float  = force(key)      .asFloat
  def float_ (key: KPathW): Opt[Float] = opt  (key).map(_.asFloat)
  def floats (key: KPathW): Seq[Float] = force(key)      .asSeq.map(_.asFloat)
  def floats_(key: KPathW): Pes[Float] = opt  (key).map(_.asSeq.map(_.asFloat))

  // ---------------------------------------------------------------------------
  def bigInt  (key: KPathW):     BigInt  = force(key)      .asBigInt
  def bigInt_ (key: KPathW): Opt[BigInt] = opt  (key).map(_.asBigInt)
  def bigInts (key: KPathW): Seq[BigInt] = force(key)      .asSeq.map(_.asBigInt)
  def bigInts_(key: KPathW): Pes[BigInt] = opt  (key).map(_.asSeq.map(_.asBigInt))

  def bigDecimal  (key: KPathW):     BigDecimal  = force(key)    .asBigDecimal
  def bigDecimal_ (key: KPathW): Opt[BigDecimal] = opt(key).map(_.asBigDecimal)
  def bigDecimals (key: KPathW): Seq[BigDecimal] = force(key)      .asSeq.map(_.asBigDecimal)
  def bigDecimals_(key: KPathW): Pes[BigDecimal] = opt  (key).map(_.asSeq.map(_.asBigDecimal))

  // time: t210202124121 - p3 - need a way to abstract date/dateTime, eg both can have a day added to
  def date  (key: KPathW):     LocalDate  = force(key)      .asLocalDate
  def date_ (key: KPathW): Opt[LocalDate] = opt  (key).map(_.asLocalDate)
  def dates (key: KPathW): Seq[LocalDate] = force(key)      .asSeq.map(_.asLocalDate)
  def dates_(key: KPathW): Pes[LocalDate] = opt  (key).map(_.asSeq.map(_.asLocalDate))

  def dateTime  (key: KPathW):     LocalDateTime  = force(key)      .asLocalDateTime
  def dateTime_ (key: KPathW): Opt[LocalDateTime] = opt  (key).map(_.asLocalDateTime)
  def dateTimes (key: KPathW): Seq[LocalDateTime] = force(key)      .asSeq.map(_.asLocalDateTime)
  def dateTimes_(key: KPathW): Pes[LocalDateTime] = opt  (key).map(_.asSeq.map(_.asLocalDateTime))

  // ===========================================================================
  //TODO: t210201095414
  def enum  [T: WTT](key: KPathW):     Double  = ???//force(key).thn(_enum)
  def enum_ [T: WTT](key: KPathW): Opt[Double] = ???//opt  (key).map(_enum)
  def enums [T: WTT](key: KPathW): Seq[Double] = ???//force(key)      .asSeq.map(_enum)
  def enums_[T: WTT/* <: EnumEntry*/](key: KPathW): Pes[Double] = ???//opt  (key).map(_.asSeq.map(_enum))

  // ---------------------------------------------------------------------------
  def text  (key: KPathW):     String  = force(key).thn(format)
  def text_ (key: KPathW): Opt[String] = opt  (key).map(format)
  def texts (key: KPathW): Seq[String] = force(key)      .asSeq.map(format)
  def texts_(key: KPathW): Pes[String] = opt  (key).map(_.asSeq.map(format))

  @gallia.NumberAbstraction
  def nmbr  (key: KPathW):     Double  = force(key).thn(_nmbr)
  def nmbr_ (key: KPathW): Opt[Double] = opt  (key).map(_nmbr)
  def nmbrs (key: KPathW): Seq[Double] = force(key)      .asSeq.map(_nmbr)
  def nmbrs_(key: KPathW): Pes[Double] = opt  (key).map(_.asSeq.map(_nmbr))

  // ---------------------------------------------------------------------------
  def obj  (key: KPathW):     Obj  = force(key)      .asObj
  def obj_ (key: KPathW): Opt[Obj] = opt  (key).map(_.asObj)
  def objs (key: KPathW): Seq[Obj] = force(key)      .asSeq.map(_.asObj)
  def objs_(key: KPathW): Pes[Obj] = opt  (key).map(_.asSeq.map(_.asObj))

  // ---------------------------------------------------------------------------
  def typed  [T: WTT](key: KPathW):     T  = force(key)                .asTyped[T]
  def typed_ [T: WTT](key: KPathW): Opt[T] = opt  (key)          .map(_.asTyped[T])
  def typeds [T: WTT](key: KPathW): Seq[T] = force(key)      .asSeq.map(_.asTyped[T])
  def typeds_[T: WTT](key: KPathW): Pes[T] = opt  (key).map(_.asSeq.map(_.asTyped[T]))

  // ===========================================================================
  def cardinality(key: KPathW): Int  = opt(key) match { // TODO: also see 210113124437
    case None            => 0
    case (some) =>
      some match {
        case seq: Seq[_] => seq.size
        case sgl         => 1 } }
}

// ===========================================================================
object ObjAccessors {

  // TODO: t201017102332
  @gallia.NumberAbstraction
  private def _nmbr(x: Any): Double = // TODO: or bigdecimal?
    x match {
      case x: Number => x.doubleValue
      case x: String => x.toDouble // TODO: error message
      case x         => ??? }      // TODO: error message

}
// ===========================================================================
