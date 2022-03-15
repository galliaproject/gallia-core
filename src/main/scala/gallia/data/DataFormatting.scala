package gallia
package data

import java.time.format._

import aptus.{String_, Double_}

// ===========================================================================
@TypeMatching object DataFormatting {

  def formatBasicValue: PartialFunction[Any, String] = formatString.orElse(formatNumber).orElse(formatOther)

    // ===========================================================================
    def formatString: PartialFunction[Any, String] = {
      case x: String    => x

      case x: EnumEntry => x.entryName
      case x: Symbol    => x.name
      case x: Char      => x.toString }

    // ---------------------------------------------------------------------------
    def formatNumber: PartialFunction[Any, String] = {
      case x: Int       => x.formatExplicit
      case x: Double    => x.formatExplicit

      case x: Long      => x.formatExplicit
      case x: Float     => x.formatExplicit

      case x: Short     => x.formatExplicit
      case x: Byte      => x.formatExplicit // TODO: to hex form?

      case x: BigInt     => x.bigInteger.toString /* stable */
      case x: BigDecimal => x.bigDecimal.toString /* stable */ }

    // ---------------------------------------------------------------------------
    def formatOther: PartialFunction[Any, String] = {
      case x: Boolean => x.toString

      //TODO: ISO_DATE* ok? - t210116162405 - read counterpart
      case x: LocalDate     => x.format(DateTimeFormatter.ISO_DATE)
      case x: LocalDateTime => x.format(DateTimeFormatter.ISO_DATE_TIME)

      case x: gallia.Whatever => x.formatDefault // TODO: keep? allow whatever here?
      case x: Unit            => "()".quote // TODO: keep?
      case gallia.none        => "null" // TODO: t210115144940

      case x =>
        // "The only difference between Java strings and Json strings is that in Json, forward-slash (/) is escaped."
        org.apache.commons.lang3.StringEscapeUtils.escapeJava(x.toString ).quote // risky... error out rather?
    }
}

// ===========================================================================
