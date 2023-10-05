package gallia
package data

import java.time.format._

import aptus.{String_, Int_, Long_, Double_, ArrayByte_}

// ===========================================================================
@TypeMatching private[gallia] object DataFormatting {
  @inline def formatBoolean(value: Boolean): String = value.toString /* to "true" or "false" */

  // ---------------------------------------------------------------------------
  // TODO: stick to printf way? must still trim spaces with %d and trailing zeros with %f
  def formatInt   (value: Int)   : String  = value         .formatExplicit
  def formatByte  (value: Byte)  : String  = value.toInt   .formatExplicit
  def formatShort (value: Short) : String  = value.toInt   .formatExplicit
  def formatLong  (value: Long)  : String  = value         .formatExplicit

  def formatDouble(value: Double): String  = value         .formatExplicit
  def formatFloat (value: Float) : String  = value.toDouble.formatExplicit

  // ===========================================================================
  def formatBigInt(value: BigInt): String = value.bigInteger.toString /* stable */
  def formatBigDec(value: BigDec): String = value.bigDecimal.toString /* stable */

  // ===========================================================================
  def formatLocalDate     (value: LocalDate)     : String = DateTimeFormatter.ISO_DATE            .format(value)
  def formatLocalTime     (value: LocalTime)     : String = DateTimeFormatter.ISO_TIME            .format(value)
  def formatLocalDateTime (value: LocalDateTime) : String = DateTimeFormatter.ISO_LOCAL_DATE_TIME .format(value)
  def formatOffsetDateTime(value: OffsetDateTime): String = DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value)
  def formatZonedDateTime (value: ZonedDateTime) : String = DateTimeFormatter.ISO_ZONED_DATE_TIME .format(value)
  def formatInstant       (value: Instant)       : String = DateTimeFormatter.ISO_INSTANT         .format(value)

  // ===========================================================================
  def formatBinary(bytes: ByteBuffer): String =
    bytes
      .array
      .toBase64
      .prepend(Base64StringPrefix)
}

// ===========================================================================
