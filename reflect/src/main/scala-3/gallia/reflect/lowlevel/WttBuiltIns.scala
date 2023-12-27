package gallia
package reflect
package lowlevel

// ===========================================================================
object WttBuiltIns extends ReflectionTypesAbstraction {

  val _String  : WTT[String]  = WTT[String] (TypeNodeBuiltIns.String, None)

  // ---------------------------------------------------------------------------
  val _Boolean : WTT[Boolean] = WTT[Boolean](TypeNodeBuiltIns.ScalaBoolean, None)
  val _Int     : WTT[Int]     = WTT[Int]    (TypeNodeBuiltIns.ScalaInt    , None)
  val _Double  : WTT[Double]  = WTT[Double] (TypeNodeBuiltIns.ScalaDouble , None)

  // ---------------------------------------------------------------------------
  val _Byte    : WTT[Byte]  = WTT[Byte] (TypeNodeBuiltIns.ScalaByte , None)
  val _Short   : WTT[Short] = WTT[Short](TypeNodeBuiltIns.ScalaShort, None)
  val _Long    : WTT[Long]  = WTT[Long] (TypeNodeBuiltIns.ScalaLong , None)

  val _Float   : WTT[Float] = WTT[Float](TypeNodeBuiltIns.ScalaFloat, None)

  // ---------------------------------------------------------------------------
  val _BigInt    : WTT[BigInt]     = WTT[BigInt]    (TypeNodeBuiltIns.ScalaMathBigInt    , None)
  val _BigDecimal: WTT[BigDecimal] = WTT[BigDecimal](TypeNodeBuiltIns.ScalaMathBigDecimal, None)

  // ---------------------------------------------------------------------------
  val _LocalDate     : WTT[java.time.LocalDate]      = WTT[java.time.LocalDate]     (TypeNodeBuiltIns.JavaTimeLocalDate     , None)
  val _LocalTime     : WTT[java.time.LocalTime]      = WTT[java.time.LocalTime]     (TypeNodeBuiltIns.JavaTimeLocalTime     , None)
  val _LocalDateTime : WTT[java.time.LocalDateTime]  = WTT[java.time.LocalDateTime] (TypeNodeBuiltIns.JavaTimeLocalDateTime , None)
  val _OffsetDateTime: WTT[java.time.OffsetDateTime] = WTT[java.time.OffsetDateTime](TypeNodeBuiltIns.JavaTimeOffsetDateTime, None)
  val _ZonedDateTime : WTT[java.time.ZonedDateTime]  = WTT[java.time.ZonedDateTime] (TypeNodeBuiltIns.JavaTimeZonedDateTime , None)
  val _Instant       : WTT[java.time.Instant]        = WTT[java.time.Instant]       (TypeNodeBuiltIns.JavaTimeInstant       , None)

  // ---------------------------------------------------------------------------
  val _ByteBuffer: WTT[java.nio.ByteBuffer] = WTT[java.nio.ByteBuffer](TypeNodeBuiltIns.JavaNioByteByffer, None) }

// ===========================================================================