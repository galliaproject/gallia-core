package gallia
package reflect

import aptus.String_

// ===========================================================================
case class FullyQualifiedName(items: Seq[String]) {
  require(items.nonEmpty)

  // ---------------------------------------------------------------------------
  private val fullName = items.mkString(".")

  val firstItem = items.head
  val  lastItem = items.last

  // ---------------------------------------------------------------------------
  override def toString: String = format

  def format     : FullNameString = fullName
  def inScopeName: String         = lastItem

  // ---------------------------------------------------------------------------
  def isJavaString: Boolean = fullName == FullNameBuiltIns._JavaString

  // ---------------------------------------------------------------------------
  def isAny       : Boolean = fullName == FullNameBuiltIns._ScalaAny

  def isOption    : Boolean = fullName == FullNameBuiltIns._Option
  def isNone      : Boolean = fullName == FullNameBuiltIns._None
  def isSome      : Boolean = fullName == FullNameBuiltIns._Some

  // ---------------------------------------------------------------------------
  def isProduct   : Boolean = fullName == FullNameBuiltIns._Product
  def isByteBuffer: Boolean = fullName == FullNameBuiltIns._ByteBuffer

  // ---------------------------------------------------------------------------
  def isGalliaEnumValue: Boolean = fullName == FullNameBuiltIns._GalliaEnumValue
  def isGalliaBObj     : Boolean = fullName == FullNameBuiltIns._GalliaBObj
  def isGalliaWhatever : Boolean = fullName == FullNameBuiltIns._GalliaWhatever

  // ===========================================================================
  private[reflect] def startsWithScalaPackage : Boolean = firstItem == FullNameBuiltIns.ScalaPackageName
  private[reflect] def startsWithJavaPackage  : Boolean = firstItem == FullNameBuiltIns. JavaPackageName
  private[reflect] def startsWithGalliaPackage: Boolean = firstItem == FullNameBuiltIns.GalliaPackageName }

// ===========================================================================
object FullyQualifiedName {
  def from(value: FullNameString): FullyQualifiedName = value.splitBy(".").pipe(FullyQualifiedName.apply)

  // ===========================================================================
  def containsSeq      (fullNames: Seq[FullNameString]): Boolean = fullNames.contains(FullNameBuiltIns._Seq)
  def containsEnumEntry(fullNames: Seq[FullNameString]): Boolean = fullNames.contains(FullNameBuiltIns._EnumEntry)

  // ===========================================================================
  @annotation.switch
  private[gallia] def normalizeFullName(value: FullyQualifiedName): FullyQualifiedName =
      FullyQualifiedName.from(normalizeFullName(value.format))

    // ---------------------------------------------------------------------------
    @annotation.switch
    private def normalizeFullName(value: FullNameString): FullNameString = // TODO: switch to non-String version
      value match {
        case FullNameBuiltIns._JavaString       => FullNameBuiltIns._JavaString // leave unchanged (TODO: use scala Predef's alias?)

        // ---------------------------------------------------------------------------
        case FullNameBuiltIns._JavaInteger      => FullNameBuiltIns._ScalaInt // all others will be handled below by the package swap (default clause)

        // ---------------------------------------------------------------------------
        case FullNameBuiltIns._BooleanPrimitive => FullNameBuiltIns._ScalaBoolean
        case FullNameBuiltIns._IntPrimitive     => FullNameBuiltIns._ScalaInt
        case FullNameBuiltIns._DoublePrimitive  => FullNameBuiltIns._ScalaDouble

        case FullNameBuiltIns._LongPrimitive    => FullNameBuiltIns._ScalaLong

        case FullNameBuiltIns._BytePrimitive    => FullNameBuiltIns._ScalaByte
        case FullNameBuiltIns._ShortPrimitive   => FullNameBuiltIns._ScalaShort
        case FullNameBuiltIns._FloatPrimitive   => FullNameBuiltIns._ScalaFloat

        // ---------------------------------------------------------------------------
        // note: no automatic conversions between scala.math.Big* and java.math.Big*, unlike the java.lang.* counterparts
        case other => other.replace(FullNameBuiltIns.JavaLangPackageNameDot, FullNameBuiltIns.ScalaPackageNameDot) /* not so for java.math (not equivalent at runtime) */ }

  // ===========================================================================
  private[reflect] def fromRuntimeValue(value: Any): FullNameString =
    value
      .getClass
      .getName // FIXME: t231018094951 - does not always agree with TypeNode's, eg "int" instead of "Scala.Int"
      .pipe(normalizeFullName) }

// ===========================================================================