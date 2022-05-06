package gallia
package data
package single

import aptus.{Opt, Pes}
import reflect.ReflectUtils.withEntryName

// ===========================================================================
@TypeMatching
trait ObjAccessors { ignored: Obj => // id210326140514
  import ObjAccessors._nmbr
  import ValueWrapper.to
  import DataFormatting.{formatBasicValue => format}
  
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

  def bigDec  (key: KPathW):     BigDec  = force(key)    .asBigDec
  def bigDec_ (key: KPathW): Opt[BigDec] = opt(key).map(_.asBigDec)
  def bigDecs (key: KPathW): Seq[BigDec] = force(key)      .asSeq.map(_.asBigDec)
  def bigDecs_(key: KPathW): Pes[BigDec] = opt  (key).map(_.asSeq.map(_.asBigDec))

  // ---------------------------------------------------------------------------
  // time: t210202124121 - p3 - need a way to abstract date/dateTime, eg both can have a day added to
  def localDate  (key: KPathW):     LocalDate  = force(key)      .asLocalDate
  def localDate_ (key: KPathW): Opt[LocalDate] = opt  (key).map(_.asLocalDate)
  def localDates (key: KPathW): Seq[LocalDate] = force(key)      .asSeq.map(_.asLocalDate)
  def localDates_(key: KPathW): Pes[LocalDate] = opt  (key).map(_.asSeq.map(_.asLocalDate))

  def localTime  (key: KPathW):     LocalTime  = force(key)      .asLocalTime
  def localTime_ (key: KPathW): Opt[LocalTime] = opt  (key).map(_.asLocalTime)
  def localTimes (key: KPathW): Seq[LocalTime] = force(key)      .asSeq.map(_.asLocalTime)
  def localTimes_(key: KPathW): Pes[LocalTime] = opt  (key).map(_.asSeq.map(_.asLocalTime))

  def localDateTime  (key: KPathW):     LocalDateTime  = force(key)      .asLocalDateTime
  def localDateTime_ (key: KPathW): Opt[LocalDateTime] = opt  (key).map(_.asLocalDateTime)
  def localDateTimes (key: KPathW): Seq[LocalDateTime] = force(key)      .asSeq.map(_.asLocalDateTime)
  def localDateTimes_(key: KPathW): Pes[LocalDateTime] = opt  (key).map(_.asSeq.map(_.asLocalDateTime))

  def offsetDateTime  (key: KPathW):     OffsetDateTime  = force(key)      .asOffsetDateTime
  def offsetDateTime_ (key: KPathW): Opt[OffsetDateTime] = opt  (key).map(_.asOffsetDateTime)
  def offsetDateTimes (key: KPathW): Seq[OffsetDateTime] = force(key)      .asSeq.map(_.asOffsetDateTime)
  def offsetDateTimes_(key: KPathW): Pes[OffsetDateTime] = opt  (key).map(_.asSeq.map(_.asOffsetDateTime))

  def zonedDateTime  (key: KPathW):     ZonedDateTime  = force(key)      .asZonedDateTime
  def zonedDateTime_ (key: KPathW): Opt[ZonedDateTime] = opt  (key).map(_.asZonedDateTime)
  def zonedDateTimes (key: KPathW): Seq[ZonedDateTime] = force(key)      .asSeq.map(_.asZonedDateTime)
  def zonedDateTimes_(key: KPathW): Pes[ZonedDateTime] = opt  (key).map(_.asSeq.map(_.asZonedDateTime))

  def instant  (key: KPathW):     Instant  = force(key)      .asInstant
  def instant_ (key: KPathW): Opt[Instant] = opt  (key).map(_.asInstant)
  def instants (key: KPathW): Seq[Instant] = force(key)      .asSeq.map(_.asInstant)
  def instants_(key: KPathW): Pes[Instant] = opt  (key).map(_.asSeq.map(_.asInstant))

  // ---------------------------------------------------------------------------
  def binary  (key: KPathW):     ByteBuffer  = force(key)      .asBinary
  def binary_ (key: KPathW): Opt[ByteBuffer] = opt  (key).map(_.asBinary)
  def binarys (key: KPathW): Seq[ByteBuffer] = force(key)      .asSeq.map(_.asBinary)
  def binarys_(key: KPathW): Pes[ByteBuffer] = opt  (key).map(_.asSeq.map(_.asBinary))

  // ---------------------------------------------------------------------------
  def enm  (key: KPathW):     EnumValue  = force(key)      .asEnum
  def enm_ (key: KPathW): Opt[EnumValue] = opt  (key).map(_.asEnum)
  def enms (key: KPathW): Seq[EnumValue] = force(key)      .asSeq.map(_.asEnum)
  def enms_(key: KPathW): Pes[EnumValue] = opt  (key).map(_.asSeq.map(_.asEnum))

  // ===========================================================================
  def enumeratum   [T <: EnumEntry: WTT](key: KPathW):     T  = enm  (key)            .stringValue.pipe(withEntryName[T])
  def enumeratum_  [T <: EnumEntry: WTT](key: KPathW): Opt[T] = enm_ (key).map(      _.stringValue.pipe(withEntryName[T]))
  def enumeratums  [T <: EnumEntry: WTT](key: KPathW): Seq[T] = enms (key)      .map(_.stringValue.pipe(withEntryName[T]))
  def enumeratums_ [T <: EnumEntry: WTT](key: KPathW): Pes[T] = enms_(key).map(_.map(_.stringValue.pipe(withEntryName[T])))

  // ---------------------------------------------------------------------------
  def text  (key: KPathW):     String  = force(key).pipe(format)
  def text_ (key: KPathW): Opt[String] = opt  (key).map (format)
  def texts (key: KPathW): Seq[String] = force(key)      .asSeq.map(format)
  def texts_(key: KPathW): Pes[String] = opt  (key).map(_.asSeq.map(format))

  // ---------------------------------------------------------------------------
  @NumberAbstraction
  def nmbr  (key: KPathW):     Double  = force(key).pipe(_nmbr)
  def nmbr_ (key: KPathW): Opt[Double] = opt  (key).map (_nmbr)
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
  @NumberAbstraction
  private def _nmbr(x: Any): Double = // TODO: or bigdecimal?
    x match {
      case x: Number => x.doubleValue
      case x: String => x.toDouble // TODO: error message
      case x         => ??? }      // TODO: error message

}
// ===========================================================================
