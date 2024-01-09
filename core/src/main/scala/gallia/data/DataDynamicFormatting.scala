package gallia
package data

import aptus._

// ===========================================================================
private[gallia] object DataDynamicFormatting {
  import DataFormatting._

  // ---------------------------------------------------------------------------
  def formatBasicValue: PartialFunction[Any, String] =
      formatString
        .orElse {
      formatNumber }
        .orElse {
      formatTemporal }
        .orElse {
      formatOther }

    // ===========================================================================
    private def formatString: PartialFunction[Any, String] = {
      case x: String    => x
      case x: EnumEntry => x.entryName
      case x: Symbol    => x.name     /* keep? */
      case x: Char      => x.toString /* keep? */}

    // ---------------------------------------------------------------------------
    private def formatNumber: PartialFunction[Any, String] = { // as in java.lang.Number
      case x: Int       => formatInt   (x)
      case x: Double    => formatDouble(x)
      case x: Byte      => formatByte  (x)
      case x: Short     => formatShort (x)
      case x: Long      => formatLong  (x)
      case x: Float     => formatFloat (x)
      case x: BigInt    => formatBigInt(x)
      case x: BigDec    => formatBigDec(x) }

    // ---------------------------------------------------------------------------
    private def formatTemporal: PartialFunction[Any, String] = {
      case x: LocalDate      => formatLocalDate(x)
      case x: LocalTime      => formatLocalTime(x)
      case x: LocalDateTime  => formatLocalDateTime (x)
      case x: OffsetDateTime => formatOffsetDateTime(x)
      case x: ZonedDateTime  => formatZonedDateTime (x)
      case x: Instant        => formatInstant(x) }

    // ---------------------------------------------------------------------------
    private def formatOther: PartialFunction[Any, String] = {
      case x: Boolean         => x.toString
      case x: ByteBuffer      => formatBinary(x)
      case _: Unit            => "()".quote // TODO: keep?
      case _: gallia.Whatever => aptus.illegalState()

      case gallia.none        => "null" // TODO: t210115144940
      case x =>
        // "The only difference between Java strings and Json strings is that in Json, forward-slash (/) is escaped."
        org.apache.commons.lang3.StringEscapeUtils.escapeJava(x.toString ).quote // risky... error out rather?
    }

}

// ===========================================================================