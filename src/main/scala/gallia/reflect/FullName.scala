package gallia
package reflect

import aptus.Anything_

// ===========================================================================
case class FullName(items: Seq[String]) { import FullName._
  private val fullName = items.mkString(".")

  def format: FullNameString = fullName

  // ---------------------------------------------------------------------------
  def isOption    : Boolean = fullName == _Option
  def isNone      : Boolean = fullName == _None
  def isSome      : Boolean = fullName == _Some

  def isProduct   : Boolean = fullName == _Product
  def isEnumValue : Boolean = fullName == _EnumValue
  def isByteBuffer: Boolean = fullName == _ByteBuffer

  // ---------------------------------------------------------------------------
  def isGalliaHeadU     : Boolean = fullName == _HeadU
  def isGalliaHeadZ     : Boolean = fullName == _HeadZ

  def isGalliaBObj     : Boolean = fullName == _BObj
  def isGalliaAObj     : Boolean = fullName == _AObj

  def isGalliaWhatever     : Boolean = fullName == _Whatever
  def isGalliaTypedWhatever: Boolean = fullName == _TypedWhatever

  // ===========================================================================
  private[reflect] def startsWithScalaPackage: Boolean = fullName.startsWith(ScalaPackageName)
  private[reflect] def startsWithJavaPackage : Boolean = fullName.startsWith( JavaPackageName)

  // ===========================================================================
  /** eg "Option" from "scala.Option[String]", or "String" from "java.lang.String" */
  private[reflect] def alias(typeSymbolString: String): Option[Alias] =
    typeSymbolString
      .takeWhile(_ != '[') /* TODO: cleaner way? */
      .pipe(FullName.simplifyFullName)
      .in.noneIf(_ == fullName) }

// ===========================================================================
object FullName {
  import aptus.{String_, Class_}

  // ---------------------------------------------------------------------------
  def from(value: FullNameString): FullName = value.splitBy(".").pipe(FullName.apply)

  // ===========================================================================
  private val ScalaPackageName        = "scala."        .intern()
  private val ScalaPackagePackageName = "scala.package.".intern()

  private val JavaPackageName     = "java."     .intern()
  private val JavaLangPackageName = "java.lang.".intern()
  private val JavaTimePackageName = "java.time.".intern()
  private val JavaMathPackageName = "java.math.".intern()

  private val EnumeratumPackageName = "enumeratum.".intern()

  // ===========================================================================
  private val _AObj: FullNameString = "gallia.domain.AObj".intern()
  private val _BObj: FullNameString = "gallia.domain.BObj".intern()

  private val _HeadU: FullNameString = "gallia.heads.HeadU".intern()
  private val _HeadZ: FullNameString = "gallia.heads.HeadZ".intern()

  // ---------------------------------------------------------------------------
  private val      _Whatever: FullNameString = "gallia.whatever.Whatever".intern()
  private val _TypedWhatever: FullNameString = "gallia.whatever.TypedWhatever".intern()

  // ---------------------------------------------------------------------------
  private[reflect] val _EnumValue: FullNameString = "gallia.EnumValue".intern()

  // ===========================================================================
  private val _Seq   : FullNameString = classOf[scala.collection.Seq[_]].fullPath.intern()
  private val _Option: FullNameString = classOf[scala.Option[_]]        .fullPath.intern()

  private val _Some  : FullNameString = classOf[scala.Some[_]].fullPath.intern()
  private val _None  : FullNameString = scala.None.getClass   .fullPath.intern()

  // ---------------------------------------------------------------------------
  private val _Product: FullNameString = classOf[scala.Product].fullPath.intern()

  // ===========================================================================
  /*private */val _JavaString : FullNameString = classOf[java.lang.String] .fullPath.intern()

  private val _JavaBoolean: FullNameString = classOf[java.lang.Boolean].fullPath.intern()
  private val _JavaInteger: FullNameString = classOf[java.lang.Integer].fullPath.intern()
  private val _JavaDouble : FullNameString = classOf[java.lang.Double ].fullPath.intern()

  private val _JavaByte : FullNameString = classOf[java.lang.Byte ].fullPath.intern()
  private val _JavaShort: FullNameString = classOf[java.lang.Short].fullPath.intern()
  private val _JavaLong : FullNameString = classOf[java.lang.Long ].fullPath.intern()

  private val _JavaFloat: FullNameString = classOf[java.lang.Float].fullPath.intern()

  // ---------------------------------------------------------------------------
  private val _JavaBigInteger: FullNameString = "java.math.BigInteger".intern()
  private val _JavaBigDecimal: FullNameString = "java.math.BigDecimal".intern()

  // ---------------------------------------------------------------------------
  private[reflect] val _JavaLocalDate     : FullNameString = "java.time.LocalDate"     .intern()
  private[reflect] val _JavaLocalTime     : FullNameString = "java.time.LocalTime"     .intern()
  private[reflect] val _JavaLocalDateTime : FullNameString = "java.time.LocalDateTime" .intern()
  private[reflect] val _JavaOffsetDateTime: FullNameString = "java.time.OffsetDateTime".intern()
  private[reflect] val _JavaZonedDateTime : FullNameString = "java.time.ZonedDateTime" .intern()
  private[reflect] val _JavaInstant       : FullNameString = "java.time.Instant"       .intern()

  // ---------------------------------------------------------------------------
  private[reflect] val _JavaByteByffer: FullNameString = "java.nio.ByteBuffer"

  // ===========================================================================
  // note: not String primitive

  private val _BooleanPrimitive: FullNameString = classOf[scala.Boolean].fullPath.ensuring(_ == "boolean").intern()
  private val _IntPrimitive    : FullNameString = classOf[scala.Int]    .fullPath.ensuring(_ == "int")    .intern()
  private val _DoublePrimitive : FullNameString = classOf[scala.Double] .fullPath.ensuring(_ == "double") .intern()

  private val _BytePrimitive   : FullNameString = classOf[scala.Byte]   .fullPath.ensuring(_ == "byte")   .intern()
  private val _ShortPrimitive  : FullNameString = classOf[scala.Short]  .fullPath.ensuring(_ == "short")  .intern()
  private val _LongPrimitive   : FullNameString = classOf[scala.Long]   .fullPath.ensuring(_ == "long")   .intern()

  private val _FloatPrimitive  : FullNameString = classOf[scala.Float]  .fullPath.ensuring(_ == "float")  .intern()

  // ===========================================================================
  private val _ScalaString : FullNameString = "scala.Predef.String".intern() // just an alias

  private[reflect]  val _ScalaBoolean: FullNameString = "scala.Boolean".intern()
  private[reflect]  val _ScalaInt    : FullNameString = "scala.Int"    .intern()
  private[reflect]  val _ScalaDouble : FullNameString = "scala.Double" .intern()

  private[reflect]  val _ScalaByte   : FullNameString = "scala.Byte"   .intern()
  private[reflect]  val _ScalaShort  : FullNameString = "scala.Short"  .intern()
  private[reflect]  val _ScalaLong   : FullNameString = "scala.Long"   .intern()

  private[reflect]  val _ScalaFloat  : FullNameString = "scala.Float"  .intern()

  // ---------------------------------------------------------------------------
  private[reflect]  val _ScalaBigInt    : FullNameString = "scala.math.BigInt"    .intern()
  private[reflect]  val _ScalaBigDecimal: FullNameString = "scala.math.BigDecimal".intern()

  // ===========================================================================
  private val _ByteBuffer: FullNameString = classOf[java.nio.ByteBuffer].fullPath.intern()

  // ===========================================================================
  private val _EnumEntry: FullNameString = classOf[enumeratum.EnumEntry].fullPath.intern()

  // ===========================================================================
  def containsSeq      (fullNames: Seq[FullNameString]): Boolean = fullNames.contains(_Seq)
  def containsEnumEntry(fullNames: Seq[FullNameString]): Boolean = fullNames.contains(_EnumEntry)

  // ===========================================================================
  private[reflect] def simplifyFullName(value: FullNameString): Alias =
    value
      .stripPrefix(JavaLangPackageName) // TODO: keep? only for String?
      .stripPrefix(JavaTimePackageName)
      .stripPrefix(JavaMathPackageName)
      .stripPrefix(ScalaPackagePackageName)
      .stripPrefix(ScalaPackageName)
      .stripPrefix(EnumeratumPackageName)

  // ===========================================================================
  @annotation.switch
  private[reflect] def normalizeFullName(value: FullNameString): FullNameString =
    value match {
      case FullName._JavaString       => FullName._JavaString // leave unchanged (TODO: use scala Predef's alias?)

      // ---------------------------------------------------------------------------
      case FullName._JavaInteger      => _ScalaInt // all others will be handled below by the package swap (default clause)

      // ---------------------------------------------------------------------------
      case FullName._BooleanPrimitive => _ScalaBoolean
      case FullName._IntPrimitive     => _ScalaInt
      case FullName._DoublePrimitive  => _ScalaDouble

      case FullName._LongPrimitive    => _ScalaLong

      case FullName._BytePrimitive    => _ScalaByte
      case FullName._ShortPrimitive   => _ScalaShort
      case FullName._FloatPrimitive   => _ScalaFloat

      // ---------------------------------------------------------------------------
      // note: no automatic conversions between scala.math.Big* and java.math.Big*, unlike the java.lang.* counterparts
      case _ => value.replace(JavaLangPackageName, ScalaPackageName) /* not so for java.math (not equivalent at runtime) */ }

  // ===========================================================================
  private[reflect] def fromRuntimeValue(value: Any): FullNameString =
    value
      .getClass
      .getName // FIXME: t231018094951 - does not always agree with TypeNode's, eg "int" instead of "Scala.Int"
      .pipe(normalizeFullName)
}

// ===========================================================================