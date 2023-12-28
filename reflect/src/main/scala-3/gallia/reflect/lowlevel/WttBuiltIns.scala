package gallia
package reflect
package lowlevel

import scala.reflect.ClassTag

// ===========================================================================
object WttBuiltIns extends ReflectionTypesAbstraction {

  val _String  : WTT[String]  = WTT[String] (TypeNodeBuiltIns.String, ClassTag(classOf[String]), None)

  // ---------------------------------------------------------------------------
  val _Boolean : WTT[Boolean] = WTT[Boolean](TypeNodeBuiltIns.ScalaBoolean, ClassTag(classOf[String]), None)
  val _Int     : WTT[Int]     = WTT[Int]    (TypeNodeBuiltIns.ScalaInt    , ClassTag(classOf[String]), None)
  val _Double  : WTT[Double]  = WTT[Double] (TypeNodeBuiltIns.ScalaDouble , ClassTag(classOf[String]), None)

  // ---------------------------------------------------------------------------
  val _Byte    : WTT[Byte]  = WTT[Byte] (TypeNodeBuiltIns.ScalaByte , ClassTag(classOf[String]), None)
  val _Short   : WTT[Short] = WTT[Short](TypeNodeBuiltIns.ScalaShort, ClassTag(classOf[String]), None)
  val _Long    : WTT[Long]  = WTT[Long] (TypeNodeBuiltIns.ScalaLong , ClassTag(classOf[String]), None)

  val _Float   : WTT[Float] = WTT[Float](TypeNodeBuiltIns.ScalaFloat, ClassTag(classOf[String]), None)

  // ---------------------------------------------------------------------------
  val _BigInt    : WTT[BigInt]     = WTT[BigInt]    (TypeNodeBuiltIns.ScalaMathBigInt    , ClassTag(classOf[String]), None)
  val _BigDecimal: WTT[BigDecimal] = WTT[BigDecimal](TypeNodeBuiltIns.ScalaMathBigDecimal, ClassTag(classOf[String]), None)

  // ---------------------------------------------------------------------------
  val _LocalDate     : WTT[java.time.LocalDate]      = WTT[java.time.LocalDate]     (TypeNodeBuiltIns.JavaTimeLocalDate     , ClassTag(classOf[String]), None)
  val _LocalTime     : WTT[java.time.LocalTime]      = WTT[java.time.LocalTime]     (TypeNodeBuiltIns.JavaTimeLocalTime     , ClassTag(classOf[String]), None)
  val _LocalDateTime : WTT[java.time.LocalDateTime]  = WTT[java.time.LocalDateTime] (TypeNodeBuiltIns.JavaTimeLocalDateTime , ClassTag(classOf[String]), None)
  val _OffsetDateTime: WTT[java.time.OffsetDateTime] = WTT[java.time.OffsetDateTime](TypeNodeBuiltIns.JavaTimeOffsetDateTime, ClassTag(classOf[String]), None)
  val _ZonedDateTime : WTT[java.time.ZonedDateTime]  = WTT[java.time.ZonedDateTime] (TypeNodeBuiltIns.JavaTimeZonedDateTime , ClassTag(classOf[String]), None)
  val _Instant       : WTT[java.time.Instant]        = WTT[java.time.Instant]       (TypeNodeBuiltIns.JavaTimeInstant       , ClassTag(classOf[String]), None)

  // ---------------------------------------------------------------------------
  val _ByteBuffer: WTT[java.nio.ByteBuffer] = WTT[java.nio.ByteBuffer](TypeNodeBuiltIns.JavaNioByteByffer, ClassTag(classOf[String]), None) }

// ===========================================================================