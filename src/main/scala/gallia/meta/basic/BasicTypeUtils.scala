package gallia
package meta
package basic

import aptus.Seq_

// ===========================================================================
object BasicTypeUtils {

  private[reflect] def createLookup(values: Seq[BasicType]): Map[FullNameString, BasicType] =
    values
      .map { x => x.fullName -> x }
      .force.map
      .withDefault { value => aptus.illegalState(s"TODO:CantFindType:201013093225:${value}") }

  // ===========================================================================
  def combine(values: Seq[BasicType]): BasicType = // TODO: subtype these 3?
    values.distinct.sortBy(_.entryName) match {
      case Seq(                   BasicType._Int                   ) => BasicType._Int
      case Seq(BasicType._Double                                   ) => BasicType._Double
      case Seq(BasicType._Double, BasicType._Int                   ) => BasicType._Double
      case Seq(                                   BasicType._String) => BasicType._String
      case Seq(_                                , BasicType._String) => BasicType._String
      case Seq(_                , _             , BasicType._String) => BasicType._String }

  // ===========================================================================
  private[reflect] def stringOrLong[T](ifString: String => T, ifLong: Long => T)(s: String): T =
    if (!s.forall(_.isDigit)) ifString(s)
    else                      ifLong  (s.toLong)

  // ---------------------------------------------------------------------------
  def doubleFitsFloat(d: Double): Boolean =
    d <= java.lang.Float.MAX_VALUE &&
    d >= java.lang.Float.MIN_VALUE

  // ---------------------------------------------------------------------------
  def doubleFitsLong(d: Double): Boolean =
    d <= java.lang.Long.MAX_VALUE &&
    d >= java.lang.Long.MIN_VALUE

  // ===========================================================================
  @TypeMatching
  def matchingSubinfos(info: meta.InfoLike)(multiple: Multiple)(value: Any): Seq[meta.SubInfo] = {

    def filter(multiple: Multiple)(basicType: BasicType.type => BasicType): Seq[meta.SubInfo] =
      info
        .union
        .filter { _ == meta.SubInfo(multiple, basicType(BasicType)) }

    // ---------------------------------------------------------------------------
    value match {
      case _: Obj     => info.union.filter(_.isNesting)

      // ===========================================================================
      case _: Boolean => filter(multiple)(_._Boolean)
      case _: String  => filter(multiple)(_._String)
      case _: Int     => filter(multiple)(_._Int)
      case _: Double  => filter(multiple)(_._Double)

      // ---------------------------------------------------------------------------
      case _: Byte    => filter(multiple)(_._Byte)
      case _: Short   => filter(multiple)(_._Short)
      case _: Long    => filter(multiple)(_._Long)
      case _: Float   => filter(multiple)(_._Float)

      // ---------------------------------------------------------------------------
      case _: BigInt  => filter(multiple)(_._BigInt)
      case _: BigDec  => filter(multiple)(_._BigDec)

      // ---------------------------------------------------------------------------
      case _:  LocalDate     => filter(multiple)(_._LocalDate)
      case _:  LocalTime     => filter(multiple)(_._LocalTime)
      case _:  LocalDateTime => filter(multiple)(_._LocalDateTime)
      case _: OffsetDateTime => filter(multiple)(_._OffsetDateTime)
      case _:  ZonedDateTime => filter(multiple)(_._ZonedDateTime)
      case _: Instant        => filter(multiple)(_._Instant)

      // ---------------------------------------------------------------------------
      case _: ByteBuffer     => filter(multiple)(_._BigDec)

      // ---------------------------------------------------------------------------
      case _: EnumValue => info.union.filter(_.isEnmMatching(multiple)) }
    }
}

// ===========================================================================
