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
  
  // ---------------------------------------------------------------------------
  @inline
  def basicValue_ (key: Key): Option[    Any ] = attemptKey (key) // for consistency
  def basicValue  (key: Key):            Any   = basicValue_(key).get
  def basicValues (key: Key):        Seq[Any]  = basicValue_(key).get  .asInstanceOf[Seq[_]]
  def basicValues_(key: Key): Option[Seq[Any]] = basicValue_(key).map(_.asInstanceOf[Seq[_]])

  @inline
  def basicValue_ (key: SKey): Option[    Any ] = attemptKey (Symbol(key)) // for consistency
  def basicValue  (key: SKey):            Any   = basicValue_(key).get
  def basicValues (key: SKey):        Seq[Any]  = basicValue_(key).get  .asInstanceOf[Seq[_]]
  def basicValues_(key: SKey): Option[Seq[Any]] = basicValue_(key).map(_.asInstanceOf[Seq[_]])

  // ---------------------------------------------------------------------------
  def string  (key: KPathW):     String  = forcePath  (key)      .asString
  def string_ (key: KPathW): Opt[String] = attemptPath(key).map(_.asString)
  def strings (key: KPathW): Seq[String] = forcePath  (key)      .asSeq.map(_.asString)
  def strings_(key: KPathW): Pes[String] = attemptPath(key).map(_.asSeq.map(_.asString))

  def int  (key: KPathW):     Int  = forcePath  (key)    .asInt
  def int_ (key: KPathW): Opt[Int] = attemptPath(key).map(_.asInt)
  def ints (key: KPathW): Seq[Int] = forcePath  (key)      .asSeq.map(_.asInt)
  def ints_(key: KPathW): Pes[Int] = attemptPath(key).map(_.asSeq.map(_.asInt))

  def double  (key: KPathW):     Double  = forcePath  (key)      .asDouble
  def double_ (key: KPathW): Opt[Double] = attemptPath(key).map(_.asDouble)
  def doubles (key: KPathW): Seq[Double] = forcePath  (key)      .asSeq.map(_.asDouble)
  def doubles_(key: KPathW): Pes[Double] = attemptPath(key).map(_.asSeq.map(_.asDouble))

  def boolean  (key: KPathW):     Boolean  = forcePath  (key)      .asBoolean
  def boolean_ (key: KPathW): Opt[Boolean] = attemptPath(key).map(_.asBoolean)
  def booleans (key: KPathW): Seq[Boolean] = forcePath  (key)      .asSeq.map(_.asBoolean)
  def booleans_(key: KPathW): Pes[Boolean] = attemptPath(key).map(_.asSeq.map(_.asBoolean))

  // ---------------------------------------------------------------------------
  def byte  (key: KPathW):     Byte  = forcePath  (key)      .asByte
  def byte_ (key: KPathW): Opt[Byte] = attemptPath(key).map(_.asByte)
  def bytes (key: KPathW): Seq[Byte] = forcePath  (key)      .asSeq.map(_.asByte)
  def bytes_(key: KPathW): Pes[Byte] = attemptPath(key).map(_.asSeq.map(_.asByte))

  def short  (key: KPathW):     Short  = forcePath  (key)    .asShort
  def short_ (key: KPathW): Opt[Short] = attemptPath(key).map(_.asShort)
  def shorts (key: KPathW): Seq[Short] = forcePath  (key)      .asSeq.map(_.asShort)
  def shorts_(key: KPathW): Pes[Short] = attemptPath(key).map(_.asSeq.map(_.asShort))

  def long  (key: KPathW):     Long  = forcePath  (key)      .asLong
  def long_ (key: KPathW): Opt[Long] = attemptPath(key).map(_.asLong)
  def longs (key: KPathW): Seq[Long] = forcePath  (key)      .asSeq.map(_.asLong)
  def longs_(key: KPathW): Pes[Long] = attemptPath(key).map(_.asSeq.map(_.asLong))

  def float  (key: KPathW):     Float  = forcePath  (key)      .asFloat
  def float_ (key: KPathW): Opt[Float] = attemptPath(key).map(_.asFloat)
  def floats (key: KPathW): Seq[Float] = forcePath  (key)      .asSeq.map(_.asFloat)
  def floats_(key: KPathW): Pes[Float] = attemptPath(key).map(_.asSeq.map(_.asFloat))

  // ---------------------------------------------------------------------------
  def bigInt  (key: KPathW):     BigInt  = forcePath  (key)      .asBigInt
  def bigInt_ (key: KPathW): Opt[BigInt] = attemptPath(key).map(_.asBigInt)
  def bigInts (key: KPathW): Seq[BigInt] = forcePath  (key)      .asSeq.map(_.asBigInt)
  def bigInts_(key: KPathW): Pes[BigInt] = attemptPath(key).map(_.asSeq.map(_.asBigInt))

  def bigDec  (key: KPathW):     BigDec  = forcePath  (key)    .asBigDec
  def bigDec_ (key: KPathW): Opt[BigDec] = attemptPath(key).map(_.asBigDec)
  def bigDecs (key: KPathW): Seq[BigDec] = forcePath  (key)      .asSeq.map(_.asBigDec)
  def bigDecs_(key: KPathW): Pes[BigDec] = attemptPath(key).map(_.asSeq.map(_.asBigDec))

  // ---------------------------------------------------------------------------
  // time: t210202124121 - p3 - need a way to abstract date/dateTime, eg both can have a day added to
  def localDate  (key: KPathW):     LocalDate  = forcePath  (key)      .asLocalDate
  def localDate_ (key: KPathW): Opt[LocalDate] = attemptPath(key).map(_.asLocalDate)
  def localDates (key: KPathW): Seq[LocalDate] = forcePath  (key)      .asSeq.map(_.asLocalDate)
  def localDates_(key: KPathW): Pes[LocalDate] = attemptPath(key).map(_.asSeq.map(_.asLocalDate))

  def localTime  (key: KPathW):     LocalTime  = forcePath  (key)      .asLocalTime
  def localTime_ (key: KPathW): Opt[LocalTime] = attemptPath(key).map(_.asLocalTime)
  def localTimes (key: KPathW): Seq[LocalTime] = forcePath  (key)      .asSeq.map(_.asLocalTime)
  def localTimes_(key: KPathW): Pes[LocalTime] = attemptPath(key).map(_.asSeq.map(_.asLocalTime))

  def localDateTime  (key: KPathW):     LocalDateTime  = forcePath  (key)      .asLocalDateTime
  def localDateTime_ (key: KPathW): Opt[LocalDateTime] = attemptPath(key).map(_.asLocalDateTime)
  def localDateTimes (key: KPathW): Seq[LocalDateTime] = forcePath  (key)      .asSeq.map(_.asLocalDateTime)
  def localDateTimes_(key: KPathW): Pes[LocalDateTime] = attemptPath(key).map(_.asSeq.map(_.asLocalDateTime))

  def offsetDateTime  (key: KPathW):     OffsetDateTime  = forcePath  (key)      .asOffsetDateTime
  def offsetDateTime_ (key: KPathW): Opt[OffsetDateTime] = attemptPath(key).map(_.asOffsetDateTime)
  def offsetDateTimes (key: KPathW): Seq[OffsetDateTime] = forcePath  (key)      .asSeq.map(_.asOffsetDateTime)
  def offsetDateTimes_(key: KPathW): Pes[OffsetDateTime] = attemptPath(key).map(_.asSeq.map(_.asOffsetDateTime))

  def zonedDateTime  (key: KPathW):     ZonedDateTime  = forcePath  (key)      .asZonedDateTime
  def zonedDateTime_ (key: KPathW): Opt[ZonedDateTime] = attemptPath(key).map(_.asZonedDateTime)
  def zonedDateTimes (key: KPathW): Seq[ZonedDateTime] = forcePath  (key)      .asSeq.map(_.asZonedDateTime)
  def zonedDateTimes_(key: KPathW): Pes[ZonedDateTime] = attemptPath(key).map(_.asSeq.map(_.asZonedDateTime))

  def instant  (key: KPathW):     Instant  = forcePath  (key)      .asInstant
  def instant_ (key: KPathW): Opt[Instant] = attemptPath(key).map(_.asInstant)
  def instants (key: KPathW): Seq[Instant] = forcePath  (key)      .asSeq.map(_.asInstant)
  def instants_(key: KPathW): Pes[Instant] = attemptPath(key).map(_.asSeq.map(_.asInstant))

  // ---------------------------------------------------------------------------
  def binary  (key: KPathW):     ByteBuffer  = forcePath  (key)      .asBinary
  def binary_ (key: KPathW): Opt[ByteBuffer] = attemptPath(key).map(_.asBinary)
  def binarys (key: KPathW): Seq[ByteBuffer] = forcePath  (key)      .asSeq.map(_.asBinary)
  def binarys_(key: KPathW): Pes[ByteBuffer] = attemptPath(key).map(_.asSeq.map(_.asBinary))

  // ---------------------------------------------------------------------------
  def enm  (key: KPathW):     EnumValue  = forcePath  (key)      .asEnum
  def enm_ (key: KPathW): Opt[EnumValue] = attemptPath(key).map(_.asEnum)
  def enms (key: KPathW): Seq[EnumValue] = forcePath  (key)      .asSeq.map(_.asEnum)
  def enms_(key: KPathW): Pes[EnumValue] = attemptPath(key).map(_.asSeq.map(_.asEnum))

  // ===========================================================================
  def enumeratum   [T <: EnumEntry: WTT](key: KPathW):     T  = enm  (key)            .stringValue.pipe(withEntryName[T])
  def enumeratum_  [T <: EnumEntry: WTT](key: KPathW): Opt[T] = enm_ (key).map(      _.stringValue.pipe(withEntryName[T]))
  def enumeratums  [T <: EnumEntry: WTT](key: KPathW): Seq[T] = enms (key)      .map(_.stringValue.pipe(withEntryName[T]))
  def enumeratums_ [T <: EnumEntry: WTT](key: KPathW): Pes[T] = enms_(key).map(_.map(_.stringValue.pipe(withEntryName[T])))

  // ---------------------------------------------------------------------------
  def text  (key: KPathW):     String  = forcePath  (key).pipe           (DataDynamicFormatting.formatBasicValue)
  def text_ (key: KPathW): Opt[String] = attemptPath(key).map            (DataDynamicFormatting.formatBasicValue)
  def texts (key: KPathW): Seq[String] = forcePath  (key)      .asSeq.map(DataDynamicFormatting.formatBasicValue)
  def texts_(key: KPathW): Pes[String] = attemptPath(key).map(_.asSeq.map(DataDynamicFormatting.formatBasicValue))

  // ---------------------------------------------------------------------------
  @NumberAbstraction
  def nmbr  (key: KPathW):     Double  = forcePath  (key).pipe(_nmbr)
  def nmbr_ (key: KPathW): Opt[Double] = attemptPath(key).map (_nmbr)
  def nmbrs (key: KPathW): Seq[Double] = forcePath  (key)      .asSeq.map(_nmbr)
  def nmbrs_(key: KPathW): Pes[Double] = attemptPath(key).map(_.asSeq.map(_nmbr))

  // ---------------------------------------------------------------------------
  def obj  (key: KPathW):     Obj  = forcePath  (key)      .asObj
  def obj_ (key: KPathW): Opt[Obj] = attemptPath(key).map(_.asObj)
  def objs (key: KPathW): Seq[Obj] = forcePath  (key)      .asSeq.map(_.asObj)
  def objs_(key: KPathW): Pes[Obj] = attemptPath(key).map(_.asSeq.map(_.asObj))

  // ---------------------------------------------------------------------------
  def typed  [T: WTT](key: KPathW):     T  = forcePath  (key)                  .asTyped[T]
  def typed_ [T: WTT](key: KPathW): Opt[T] = attemptPath(key)            .map(_.asTyped[T])
  def typeds [T: WTT](key: KPathW): Seq[T] = forcePath  (key)      .asSeq.map(_.asTyped[T])
  def typeds_[T: WTT](key: KPathW): Pes[T] = attemptPath(key).map(_.asSeq.map(_.asTyped[T]))

  // ===========================================================================
  def cardinality(key: KPathW): Int  = attemptPath(key) match { // TODO: also see 210113124437
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
