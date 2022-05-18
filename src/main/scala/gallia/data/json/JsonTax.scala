package gallia
package data
package json

// ===========================================================================
sealed trait JsonTax extends EnumEntry with HasTaxOpt

  // ---------------------------------------------------------------------------
  /** costly due to the very limited set of primitives datatypes supported by JSON */
  object JsonTax extends Enum[JsonTax] with HasTaxes { // 220406110635
    import JsonTaxUtils._

    val values = findValues

    // ---------------------------------------------------------------------------
    case object JsonIntTax            extends DoubleTax(BasicType._Int)   with JsonTax

    case object JsonByteTax           extends DoubleTax(BasicType._Byte)  with JsonTax
    case object JsonShortTax          extends DoubleTax(BasicType._Short) with JsonTax
    case object JsonLongTax           extends DoubleTax(BasicType._Long)  with JsonTax
    case object JsonFloatTax          extends DoubleTax(BasicType._Float) with JsonTax

    case object JsonBigIntTax         extends JsonTax { def valueOpt(c: Cls) = _tax[Any](c)(_.hasBigInt)(stringOrLong  (BigInt    .apply, BigInt    .apply)) }
    case object JsonBigDecTax         extends JsonTax { def valueOpt(c: Cls) = _tax[Any](c)(_.hasBigDec)(stringOrDouble(BigDecimal.apply, BigDecimal.apply)) }

    case object JsonLocalDateTax      extends JsonTax { def valueOpt(c: Cls) = _tax[Any](c)(_.hasLocalDate)    (stringOrLong(BasicType._LocalDate    .pair)) }
    case object JsonLocalDateTimeTax  extends JsonTax { def valueOpt(c: Cls) = _tax[Any](c)(_.hasLocalDateTime)(stringOrLong(BasicType._LocalDateTime.pair)) }
    case object JsonLocalTimeTax      extends StringTax(BasicType._LocalTime)      with JsonTax
    case object JsonOffsetDateTimeTax extends StringTax(BasicType._OffsetDateTime) with JsonTax
    case object JsonZonedDateTimeTax  extends StringTax(BasicType._ZonedDateTime)  with JsonTax
    case object JsonInstantTax        extends JsonTax { def valueOpt(c: Cls) = _tax[Any](c)(_.hasInstant)      (stringOrLong(BasicType._Instant      .pair)) }

    case object JsonBinaryTax         extends StringTax(BasicType._Binary)         with JsonTax

    case object JsonEnumTax           extends StringEnumTax                        with JsonTax
  }

// ===========================================================================
