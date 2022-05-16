package gallia
package reflect

// ===========================================================================
object CustomOrdering {
  private implicit val _arrayOrdering = aptus.arrayOrdering[Byte]

  // ---------------------------------------------------------------------------
  def localDate    : Ordering[LocalDate]       = Ordering.by(_.toEpochDay)
  def localTime    : Ordering[LocalTime]       = Ordering.by(_.toNanoOfDay)

  // ---------------------------------------------------------------------------
  def  localDateTime: Ordering[ LocalDateTime] = Ordering.by(_.toInstant(java.time.ZoneOffset.UTC))
  def offsetDateTime: Ordering[OffsetDateTime] = Ordering.by(_.toInstant)
  def  zonedDateTime: Ordering[ ZonedDateTime] = Ordering.by(_.toInstant)

  // ---------------------------------------------------------------------------
  def byteBuffer    : Ordering[ByteBuffer]     = Ordering.by(_.array)
  def standAloneObj : Ordering[Obj]            = Ordering.by(_.formatCompactJson)

  // ---------------------------------------------------------------------------
  def enumEntry     : Ordering[EnumEntry]      = Ordering.by(_.entryName)

  // ---------------------------------------------------------------------------
  def enumValue     : Ordering[Seq[EnumValue]]      = Ordering.by((x: Seq[EnumValue]) => x.map(_.stringValue))(aptus.seqOrdering)
}

// ===========================================================================