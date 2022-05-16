package gallia
package data

// ===========================================================================
sealed trait JdbcTax extends EnumEntry with HasTaxOpt

// ---------------------------------------------------------------------------
object JdbcTax extends Enum[JdbcTax] with HasTaxes { // 220513093958
  val values = findValues

  // ---------------------------------------------------------------------------
  case object JdbcBigIntTax         extends JdbcTax { def valueOpt(c: Cls) = _tax[java.math.BigInteger](c)(_.hasBigInt)(BigInt    .apply) }
  case object JdbcBigDecTax         extends JdbcTax { def valueOpt(c: Cls) = _tax[java.math.BigDecimal](c)(_.hasBigDec)(BigDecimal.apply) }

  case object JdbcLocalDateTax      extends JdbcTax { def valueOpt(c: Cls) = _tax[java.sql.Date]     (c)(_.hasLocalDate)    (_.toLocalDate) }
  case object JdbcLocalDateTimeTax  extends JdbcTax { def valueOpt(c: Cls) = _tax[java.sql.Timestamp](c)(_.hasInstant)  (_.toInstant) }
  case object JdbcLocalTimeTax      extends JdbcTax { def valueOpt(c: Cls) = _tax[java.sql.Time]     (c)(_.hasLocalTime)    (_.toLocalTime) }

  case object JdbcBinaryTax         extends JdbcTax { def valueOpt(c: Cls) = _tax[Array[Byte]](c)(_.hasBinary)(gallia.byteBuffer) }
}

// ===========================================================================