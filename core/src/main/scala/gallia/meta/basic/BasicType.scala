package gallia
package meta
package basic

import java.time._
import aptus.{Long_, String_}
import data.{DataFormatting, DataParsing}
import reflect.{FullName, TypeNodeBuiltIns}

// ===========================================================================
sealed trait NumericalType extends BasicType

  // ---------------------------------------------------------------------------
  sealed trait UnboundedNumber extends NumericalType                     // bignums
  sealed trait   BoundedNumber extends NumericalType with HasParseDouble // excluding bignums

    // ---------------------------------------------------------------------------
    sealed trait IntegerLikeType extends BoundedNumber { // basically: byte, short, int & long
        // especially useful for JSON (since doesn't differentiate integers)
        def toIntegerLike(value: Double): Any /* eg value.toLong for _Long */ }

    // ---------------------------------------------------------------------------
    sealed trait RealLikeType extends BoundedNumber { // basically: float & double
        // especially useful for JSON (since doesn't differentiate integers)
        def toRealLike(value: Double): Any /* eg value.toDouble for _Float */ }

// ===========================================================================
sealed trait HasParseString {
    type T

    // ---------------------------------------------------------------------------
          def parseString     : String => T
    final def parseStringAsAny: Any    => T = value => parseString(value.asInstanceOf[String]) }

  // ===========================================================================
  sealed trait HasParseDouble {
    type T

    // ---------------------------------------------------------------------------
          def parseDouble     : Double => T
    final def parseAnyToDouble: Any    => T = value => parseDouble(value.asInstanceOf[Double]) }

// ===========================================================================
sealed trait HasFormatString {
  type T

  // ---------------------------------------------------------------------------
        def formatString     : T   => String
  final def formatStringAsAny: Any => String = value => formatString(value.asInstanceOf[T]) }

// ===========================================================================
sealed trait HasPair { x: HasParseString =>
  type T
  def pair: (String => T, Long => T)
  final override def parseString = { val (ifString, ifLong) = pair; BasicTypeUtils.stringOrLong(ifString, ifLong)(_) } }

// ---------------------------------------------------------------------------
sealed trait HasFieldHasType { def has: Fld => Boolean }

// ---------------------------------------------------------------------------
trait HasValuePredicate { def valuePredicate: AnyValue => Boolean }

// ===========================================================================
sealed trait ParameterizedBasicType
      extends BasicType

  // ---------------------------------------------------------------------------
  sealed trait UnparameterizedBasicType
      extends BasicType
      with    HasParseString
      with    HasFieldHasType
      with    HasValuePredicate {
    final override def has: Fld => Boolean = _.hasBasicType(this)
    def valuePredicate: AnyValue => Boolean }

// ===========================================================================
// note: intentionally not called "PrimitiveType"
sealed trait BasicType // TODO: t210125111338 - investigate union types (coming in scala 3?)
      extends EnumEntry
      with    BasicTypeHelper
      with    HasFormatString
      with    meta.ValueType {
    type T

    protected[basic]       val node          : TypeNode
    protected[basic]       val ordinal       : Int // scala 3
    protected[basic] final def fullNameString: FullNameString = node.leaf.name
                     final def fullName      : FullName       = node.leaf.fullName

    def formatScala  : String = fullName.format.replace("scala.", "").replace("java.lang.String", "String") // TODO
    def formatDefault: String = entryName }

  // ===========================================================================
  @TypeMatching object BasicType extends Enum[BasicType]  {
    @deprecated val values        = findValues
                val orderedValues = findValues.sortBy(_.ordinal).distinct // TODO: distinct necessary with scala-3?

    private[gallia] lazy val fullNames  : Seq[reflect.FullName] = orderedValues.map(_.node.leaf.fullName) :+ _Enm.Node.leaf.fullName
    private[gallia] lazy val fullNameSet: Set[reflect.FullName] = fullNames.toSet

    // ---------------------------------------------------------------------------
    def fromFullNameOpt(value: FullNameString): Option[BasicType] = lookup.get     (reflect.FullName.normalizeFullName(value))
    def fromFullName   (value: FullNameString):        BasicType  = lookup.apply   (reflect.FullName.normalizeFullName(value))
    def isKnown        (value: FullNameString):        Boolean    = lookup.contains(reflect.FullName.normalizeFullName(value))

    // ---------------------------------------------------------------------------
      private val lookup: Map[FullNameString, BasicType] = BasicTypeUtils.createLookup(orderedValues)

    // ---------------------------------------------------------------------------
    @inline def matchingSubinfos(info: meta.InfoLike)(multiple: Multiple)(value: Any): Seq[meta.SubInfo] =
      BasicTypeUtils.matchingSubinfos(info)(multiple)(value)

    // ---------------------------------------------------------------------------
    private type CT[T] = scala.reflect.ClassTag[T] // just to help with boilerplate

    // ---------------------------------------------------------------------------
    // TODO: t240208161247 - create a proper placeholder rather, so can easily identify this use case (eg .forceAny("f"))
    private[gallia] val ScalaAnyPlaceHolder = _Boolean // 240208154224 - arbitrarily use _Boolean as placeholder for Any (for internal use only)
    private[gallia] val ScalaAnyFullName    = "scala.Any"

    // ===========================================================================
    // TODO:
    // - t210108114447 - support own "flag" type?
    // - t210109142406 - dedicated matrix/tensor object (dense/sparse); look into existing libraries
    // - t210110094829 - accept Obj as value, albeit the standalone version (see t210104164037)
    // - t210110095252 - CLOB
    // - change names upon serialization (eg "string" instead of _String)
    // - t220513135713 - provide built-in enum: "Char", gender, nucleotides, ...

    // ---------------------------------------------------------------------------
    case object _String extends UnparameterizedBasicType {
      type T = String

      override val  node           = TypeNodeBuiltIns.String
      override val  ordinal        = 0

      // ---------------------------------------------------------------------------
      override val   parseString   = identity
      override val  formatString   = identity
      override val  valuePredicate = _.isInstanceOf[T]
      // ---------------------------------------------------------------------------
      // unpacked boilerplate:

        override lazy val _ctag: CT[                T  ] = ctag[                T  ]
        override lazy val nctag: CT[       Iterable[T] ] = ctag[       Iterable[T] ]
        override lazy val octag: CT[       Option  [T] ] = ctag[       Option  [T] ]
        override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]

        // ---------------------------------------------------------------------------
        override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]
        override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse /* TODO: cost of reverse? */ }

    // ===========================================================================
    case object _Boolean extends UnparameterizedBasicType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = Boolean

      override val node    = TypeNodeBuiltIns.ScalaBoolean
      override val ordinal = 1

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatBoolean
      override val parseString  = _.toBoolean }

    // ===========================================================================
    case object _Int extends UnparameterizedBasicType with IntegerLikeType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = Int

      override val node    = TypeNodeBuiltIns.ScalaInt
      override val ordinal = 2

      def toIntegerLike(value: Double) = value.toInt

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatInt
      override val parseString  = _.toInt
      override val parseDouble  = d => d.toInt.ensuring(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Double extends UnparameterizedBasicType with RealLikeType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = Double

      override val node    = TypeNodeBuiltIns.ScalaDouble
      override val ordinal = 3

      def toRealLike(value: Double) = value

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatDouble
      override val parseString  = _.toDouble
      override val parseDouble  = identity }

    // ===========================================================================
    //TODO: rename these to Int{8, 16, 64}?
    case object _Byte  extends UnparameterizedBasicType with IntegerLikeType   { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = Byte

      override val node    = TypeNodeBuiltIns.ScalaByte
      override val ordinal = 4

      def toIntegerLike(value: Double) = value.toByte

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatByte
      override val parseString  = _.toByte
      override val parseDouble  = d => d.toByte.ensuring(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Short extends UnparameterizedBasicType with IntegerLikeType   { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toShort
      type T = Short

      override val node    = TypeNodeBuiltIns.ScalaShort
      override val ordinal = 5

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatShort
      override val parseString  = _.toShort
      override val parseDouble  = d => d.toShort.ensuring(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Long  extends UnparameterizedBasicType with IntegerLikeType   { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toLong
      type T = Long

      override val node    = TypeNodeBuiltIns.ScalaLong
      override val ordinal = 6

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatLong
      override val parseString  = _.toLong
      override val parseDouble  = d => d.ensuring(BasicTypeUtils.doubleFitsLong(_)) .toLong.ensuring(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Float extends UnparameterizedBasicType with RealLikeType      { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toRealLike   (value: Double) = value.toFloat
      type T = Float

      override val node    = TypeNodeBuiltIns.ScalaFloat
      override val ordinal = 7

      override val valuePredicate = _.isInstanceOf[T]
      override val formatString = DataFormatting.formatFloat
      override val parseString  = _.toFloat
      override val parseDouble  = _.ensuring(BasicTypeUtils.doubleFitsFloat(_)).toFloat /* note: precision may also be affected */ }

    // ===========================================================================
    case object _BigInt extends UnparameterizedBasicType with UnboundedNumber { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = BigInt

      override val node    = TypeNodeBuiltIns.ScalaMathBigInt
      override val ordinal = 8

      override val valuePredicate = _.isInstanceOf[T]
      override val  parseString = BigInt.apply
      override val formatString = DataFormatting.formatBigInt }

    // ---------------------------------------------------------------------------
    case object _BigDec extends UnparameterizedBasicType with UnboundedNumber { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = BigDecimal

      override val node    = TypeNodeBuiltIns.ScalaMathBigDecimal
      override val ordinal = 9

      override val valuePredicate = _.isInstanceOf[T]
      override val  parseString = BigDecimal.apply
      override val formatString = DataFormatting.formatBigDec

      override protected def accessorNameModifier(value: FullNameString): String = value.replace("imal", "") }

    // ===========================================================================
    case object _LocalDate extends UnparameterizedBasicType with HasPair { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T = LocalDate

        override val node    = TypeNodeBuiltIns.JavaTimeLocalDate
        override val ordinal = 10

        override val valuePredicate = _.isInstanceOf[T]
        override val pair         = (DataParsing   . parseLocalDate, _.toLocalDate /* aptus' */)
        override val formatString =  DataFormatting.formatLocalDate

        private implicit val ord: Ordering[T] = CustomOrdering.localDate }

      // ---------------------------------------------------------------------------
      case object _LocalTime extends UnparameterizedBasicType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T = LocalTime

        override val node    = TypeNodeBuiltIns.JavaTimeLocalTime
        override val ordinal = 11

        override val valuePredicate = _.isInstanceOf[T]
        override val  parseString = DataParsing   . parseLocalTime
        override val formatString = DataFormatting.formatLocalTime

        private implicit val ord: Ordering[T] = CustomOrdering.localTime }

      // ---------------------------------------------------------------------------
      case object _LocalDateTime extends UnparameterizedBasicType with HasPair { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T = LocalDateTime

        override val node    = TypeNodeBuiltIns.JavaTimeLocalDateTime
        override val ordinal = 12

        override val valuePredicate = _.isInstanceOf[T]
        override val pair           = (DataParsing   . parseLocalDateTime, _.toLocalDateTime /* aptus' */)
        override val formatString   =  DataFormatting.formatLocalDateTime

        private implicit val ord: Ordering[T] = CustomOrdering.localDateTime }

    // ===========================================================================
    case object _OffsetDateTime extends UnparameterizedBasicType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T = OffsetDateTime

        override val node    = TypeNodeBuiltIns.JavaTimeOffsetDateTime
        override val ordinal = 13

        override val valuePredicate = _.isInstanceOf[T]
        override val  parseString = DataParsing   . parseOffsetDateTime
        override val formatString = DataFormatting.formatOffsetDateTime

        private implicit val ord: Ordering[T] = CustomOrdering.offsetDateTime }

      // ---------------------------------------------------------------------------
      case object _ZonedDateTime extends UnparameterizedBasicType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T = ZonedDateTime

        override val node    = TypeNodeBuiltIns.JavaTimeZonedDateTime
        override val ordinal = 14

        override val valuePredicate = _.isInstanceOf[T]
        override val  parseString = DataParsing   . parseZonedDateTime
        override val formatString = DataFormatting.formatZonedDateTime

        private implicit val ord: Ordering[T] = CustomOrdering.zonedDateTime }

      // ---------------------------------------------------------------------------
      case object _Instant extends UnparameterizedBasicType with HasPair { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T = Instant

        override val node    = TypeNodeBuiltIns.JavaTimeInstant
        override val ordinal = 15

        override val valuePredicate = _.isInstanceOf[T]
        override val pair         = (DataParsing   . parseInstant, _.toInstant /* aptus' */)
        override val formatString =  DataFormatting.formatInstant

        private implicit val ord: Ordering[T] = Ordering.by(identity) /* not sure why needed */ }

    // ===========================================================================
    case object _Binary extends UnparameterizedBasicType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      type T = ByteBuffer

      override val node    = TypeNodeBuiltIns.JavaNioByteByffer
      override val ordinal = 16

      override val valuePredicate = _.isInstanceOf[T]
      override val  parseString = DataParsing   . parseBinary
      override val formatString = DataFormatting.formatBinary

      private implicit val ord: Ordering[T] = CustomOrdering.byteBuffer }

    // ===========================================================================
    // TODO:
    // - t210330102827 - capture enum name for macros (currently stored in Fld hackily)
    case class _Enm(values: Seq[EnumValue]) extends ParameterizedBasicType { /* boilerplate: */ override lazy val _ctag: CT[T] = ctag[T]; override lazy val nctag: CT[Iterable[T]] = ctag[Iterable[T]]; override lazy val octag: CT[Option [T]] = ctag[Option [T]]; override lazy val pctag: CT[Option[Iterable[T]]] = ctag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        vldt.MetaValidation.checkAreValidEnumValues(values).ensuring(_.isEmpty /* ie no errors reported */)

        override def formatDefault: String = entryName.colon(values.map(_.stringValue.surroundWith("|")).mkString(","))

        type T = gallia.EnumValue

        override val node    = _Enm.Node
        override val ordinal = 17

        def stringValues: Seq[EnumStringValue] = values.map(_.stringValue)

        override val valuePredicate = _.isInstanceOf[EnumValue]
        override val formatString   = _.stringValue

        // ---------------------------------------------------------------------------
        override protected def accessorNameModifier(value: FullNameString): String = "enm"
        private implicit val ord: Ordering[T] = CustomOrdering.enumValue }

      // ===========================================================================
      object _Enm extends HasFieldHasType with HasParseString {
        type T = EnumValue
        override def has         = _.hasEnum
        override def parseString = EnumValue.apply

        val Node = TypeNodeBuiltIns.GalliaEnumValue

        // ---------------------------------------------------------------------------
        private[gallia] val Dummy = _Enm(Seq(EnumValue("_"))) /* useful for internal comparisons in validation, see 220506101842 */ }
  }

// ===========================================================================
