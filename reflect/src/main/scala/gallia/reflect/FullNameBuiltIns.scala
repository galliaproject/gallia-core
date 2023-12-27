package gallia
package reflect

import aptus.{String_, Class_}

// ===========================================================================
object FullNameBuiltIns {
  private[reflect] val ScalaPackageName        = "scala"    .intern()
  private[reflect] val JavaPackageName         = "java"     .intern()
  private[reflect] val JavaLangPackageName     = "java.lang".intern()
  private[reflect] val GalliaPackageName       = "gallia"   .intern()

  // ---------------------------------------------------------------------------
  private[reflect] val ScalaPackageNameDot    = ScalaPackageName   .dot.intern()
  private[reflect] val JavaLangPackageNameDot = JavaLangPackageName.dot.intern()

  // ===========================================================================
  private[reflect] val _Seq   : FullNameString = classOf[scala.collection.Seq[_]].fullPath.intern()
  private[reflect] val _Option: FullNameString = classOf[scala.Option[_]]        .fullPath.intern()

  private[reflect] val _Some  : FullNameString = classOf[scala.Some[_]].fullPath.intern()
  private[reflect] val _None  : FullNameString = scala.None.getClass   .fullPath.intern()

  // ---------------------------------------------------------------------------
  private[reflect] val _Product: FullNameString = classOf[scala.Product].fullPath.intern()

  // ===========================================================================
  private[reflect] val _JavaString : FullNameString = classOf[java.lang.String] .fullPath.intern()
  private[reflect] val _JavaInteger: FullNameString = classOf[java.lang.Integer].fullPath.intern()

  // ---------------------------------------------------------------------------
  private[gallia] val _JavaTimeLocalDate     : FullNameString = "java.time.LocalDate"     .intern()
  private[gallia] val _JavaTimeLocalTime     : FullNameString = "java.time.LocalTime"     .intern()
  private[gallia] val _JavaTimeLocalDateTime : FullNameString = "java.time.LocalDateTime" .intern()
  private[gallia] val _JavaTimeOffsetDateTime: FullNameString = "java.time.OffsetDateTime".intern()
  private[gallia] val _JavaTimeZonedDateTime : FullNameString = "java.time.ZonedDateTime" .intern()
  private[gallia] val _JavaTimeInstant       : FullNameString = "java.time.Instant"       .intern()

  // ---------------------------------------------------------------------------
  private[gallia] val _JavaNioByteByffer = "java.nio.ByteBuffer".intern()

  // ===========================================================================
  // note: not String primitive

  private[reflect] val _BooleanPrimitive: FullNameString = classOf[scala.Boolean].fullPath.ensuring(_ == "boolean").intern()
  private[reflect] val _IntPrimitive    : FullNameString = classOf[scala.Int]    .fullPath.ensuring(_ == "int")    .intern()
  private[reflect] val _DoublePrimitive : FullNameString = classOf[scala.Double] .fullPath.ensuring(_ == "double") .intern()

  private[reflect] val _BytePrimitive   : FullNameString = classOf[scala.Byte]   .fullPath.ensuring(_ == "byte")   .intern()
  private[reflect] val _ShortPrimitive  : FullNameString = classOf[scala.Short]  .fullPath.ensuring(_ == "short")  .intern()
  private[reflect] val _LongPrimitive   : FullNameString = classOf[scala.Long]   .fullPath.ensuring(_ == "long")   .intern()

  private[reflect] val _FloatPrimitive  : FullNameString = classOf[scala.Float]  .fullPath.ensuring(_ == "float")  .intern()

  // ===========================================================================
  private[gallia]  val _ScalaBoolean: FullNameString = "scala.Boolean".intern()
  private[gallia]  val _ScalaInt    : FullNameString = "scala.Int"    .intern()
  private[gallia]  val _ScalaDouble : FullNameString = "scala.Double" .intern()

  private[gallia]  val _ScalaByte   : FullNameString = "scala.Byte"   .intern()
  private[gallia]  val _ScalaShort  : FullNameString = "scala.Short"  .intern()
  private[gallia]  val _ScalaLong   : FullNameString = "scala.Long"   .intern()

  private[gallia]  val _ScalaFloat  : FullNameString = "scala.Float"  .intern()

  // ---------------------------------------------------------------------------
  private[gallia]  val _ScalaMathBigInt    : FullNameString = "scala.math.BigInt"    .intern()
  private[gallia]  val _ScalaMathBigDecimal: FullNameString = "scala.math.BigDecimal".intern()

  // ---------------------------------------------------------------------------
  private[reflect] val _ScalaAnyVal: FullNameString = "scala.AnyVal"

  private[reflect] val _ScalaOption: FullNameString = "scala.Option"
  private[reflect] val _ScalaSeq   : FullNameString = "scala.collection.immutable.Seq"

  // ===========================================================================
  private[reflect] val _ByteBuffer: FullNameString = "java.nio.ByteBuffer".intern()

  // ---------------------------------------------------------------------------
  private[reflect] val _EnumEntry: FullNameString = "enumeratum.EnumEntry".intern()

  // ===========================================================================
  private[reflect] val _GalliaEnumValue: FullNameString = s"${ScalaVersion.galliaPackageName}.EnumValue".intern()
  private[reflect] val _GalliaAObj     : FullNameString = "gallia.domain.AObj"        .intern()
  private[reflect] val _GalliaBObj     : FullNameString = "gallia.domain.BObj"        .intern()
  private[reflect] val _GalliaWhatever : FullNameString = "gallia.whatever.Whatever"  .intern() }

// ===========================================================================