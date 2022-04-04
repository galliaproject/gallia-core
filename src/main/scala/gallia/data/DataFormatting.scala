package gallia
package data

import java.time.format._

import aptus.{String_, Double_, ArrayByte_}

// ===========================================================================
@TypeMatching object DataFormatting {

  def formatBasicValue: PartialFunction[Any, String] =
      formatString
        .orElse {
      formatNumber }
        .orElse {
      formatTemporal }
        .orElse {
      formatOther }

    // ===========================================================================
    private[gallia] def formatString: PartialFunction[Any, String] = {
      case x: String    => x

      case x: EnumEntry => x.entryName
      case x: Symbol    => x.name     /* keep? */
      case x: Char      => x.toString /* keep? */}

    // ---------------------------------------------------------------------------
    private[gallia] def formatNumber: PartialFunction[Any, String] = { // as in java.lang.Number
      case x: Int       => x.formatExplicit
      case x: Double    => x.formatExplicit

      case x: Long      => x.formatExplicit
      case x: Float     => x.formatExplicit

      case x: Short     => x.formatExplicit
      case x: Byte      => x.formatExplicit
      
      case x: BigInt    => formatBigInt(x)
      case x: BigDec    => formatBigDec(x) }

    // ---------------------------------------------------------------------------
    private[gallia] def formatTemporal: PartialFunction[Any, String] = {
      case x: LocalDate      => DateTimeFormatter.ISO_DATE            .format(x)
      case x: LocalTime      => DateTimeFormatter.ISO_TIME            .format(x)
      case x: LocalDateTime  => DateTimeFormatter.ISO_LOCAL_DATE_TIME .format(x)
      case x: OffsetDateTime => DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(x)
      case x: ZonedDateTime  => DateTimeFormatter.ISO_ZONED_DATE_TIME .format(x)
      case x: Instant        => DateTimeFormatter.ISO_INSTANT         .format(x) }
      
    // ---------------------------------------------------------------------------
    private[gallia] def formatOther: PartialFunction[Any, String] = {
      case x: Boolean         => x.toString
      case x: ByteBuffer      => formatBinary(x)
      case x: Unit            => "()".quote // TODO: keep?
      case x: gallia.Whatever => aptus.illegalState()

      case gallia.none        => "null" // TODO: t210115144940
      case x =>
        // "The only difference between Java strings and Json strings is that in Json, forward-slash (/) is escaped."
        org.apache.commons.lang3.StringEscapeUtils.escapeJava(x.toString ).quote // risky... error out rather?
    }

    // ===========================================================================    
    private[gallia] def formatBigInt(value: BigInt): String = value.bigInteger.toString /* stable */
    private[gallia] def formatBigDec(value: BigDec): String = value.bigDecimal.toString /* stable */
      
    // ---------------------------------------------------------------------------
    private[gallia] def formatBinary(bytes: ByteBuffer): String =
      bytes
        .array
        .toBase64
        .prepend("base64:")
}

// ===========================================================================
