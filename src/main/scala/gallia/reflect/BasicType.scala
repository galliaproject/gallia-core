package gallia
package reflect

import scala.reflect.{classTag, ClassTag}
import java.time._

import aptus.{Anything_, Seq_, String_, Long_}

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
sealed trait HasParseString { type T; def parseString: String => T }

  // ---------------------------------------------------------------------------
  sealed trait HasParseDouble { type T; def parseDouble: Double => T }

  // ---------------------------------------------------------------------------
  sealed trait HasPair { x: HasParseString =>
    type T
    def pair: (String => T, Long => T)
    final override def parseString = { val (ifString, ifLong) = pair; BasicTypeUtils.stringOrLong(ifString, ifLong)(_) }
  }

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
      with    meta.Containee {
    type T
    val  fullName: FullName }

  // ===========================================================================
  @TypeMatching object BasicType extends Enum[BasicType]  {
    val values = findValues

    // ---------------------------------------------------------------------------
    def fromFullNameOpt(value: FullName): Option[BasicType] = lookup.get     (BasicTypeUtils.normalizeFullName(value))
    def fromFullName   (value: FullName):        BasicType  = lookup.apply   (BasicTypeUtils.normalizeFullName(value))
    def isKnown        (value: FullName):        Boolean    = lookup.contains(BasicTypeUtils.normalizeFullName(value))

    // ---------------------------------------------------------------------------
      private val lookup: Map[FullName, BasicType] = BasicTypeUtils.createLookup(values)

    // ===========================================================================
    // TODO:
    // - t210108114447 - support own "flag" type?
    // - t210109142406 - dedicated matrix/tensor object (dense/sparse); look into existing libraries
    // - t210110094829 - accept Obj as value, albeit the standalone version (see t210104164037)
    // - t210110095252 - CLOB
    // - change names upon serialization (eg "string" instead of _String)
    // - t220513135713 - provide built-in "Char" enum

    // ---------------------------------------------------------------------------
    case object _String extends UnparameterizedBasicType {
      override type T              =            String
      override val  fullName       = "java.lang.String"
      override val  parseString    = identity
      override val  valuePredicate = _.isInstanceOf[T]

      // ---------------------------------------------------------------------------
      // unpacked boilerplate:

        override lazy val  ctag: ClassTag[                T  ] = classTag[                T  ]
        override lazy val nctag: ClassTag[       Iterable[T] ] = classTag[       Iterable[T] ]
        override lazy val octag: ClassTag[       Option  [T] ] = classTag[       Option  [T] ]
        override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]

        // ---------------------------------------------------------------------------
        override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]
        override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse // TODO: cost of reverse?
    }

    // ===========================================================================
    case object _Boolean extends UnparameterizedBasicType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Boolean
      val  fullName = "scala.Boolean"

      override val parseString = _.toBoolean }

    // ===========================================================================
    case object _Int extends UnparameterizedBasicType with IntegerLikeType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toInt; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Int
      val  fullName = "scala.Int"

      override val parseString = _.toInt
      override val parseDouble = d => d.toInt.assert(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Double extends UnparameterizedBasicType with RealLikeType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toRealLike   (value: Double) = value; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Double
      val  fullName = "scala.Double"

      override val parseString = _.toDouble
      override val parseDouble = identity }

    // ===========================================================================
    //TODO: rename these to Int{8, 16, 64}?
    case object _Byte  extends UnparameterizedBasicType with IntegerLikeType   { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toByte; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Byte
      val  fullName = "scala.Byte"

      override val parseString = _.toByte
      override val parseDouble = d => d.toByte.assert(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Short extends UnparameterizedBasicType with IntegerLikeType   { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toShort; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Short
      val  fullName = "scala.Short"

      override val parseString = _.toShort
      override val parseDouble = d => d.toShort.assert(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Long  extends UnparameterizedBasicType with IntegerLikeType   { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toLong; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Long
      val  fullName = "scala.Long"

      override val parseString = _.toLong
      override val parseDouble = d => d.assert(BasicTypeUtils.doubleFitsLong) .toLong.assert(_.toDouble == d) }

    // ---------------------------------------------------------------------------
    case object _Float extends UnparameterizedBasicType with RealLikeType      { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toRealLike   (value: Double) = value.toFloat; override val valuePredicate = _.isInstanceOf[T]
      type T        =        Float
      val  fullName = "scala.Float"

      override val parseString = _.toFloat
      override val parseDouble = _.assert(BasicTypeUtils.doubleFitsFloat).toFloat /* note: precision may also be affected */ }

    // ===========================================================================
    case object _BigInt extends UnparameterizedBasicType with UnboundedNumber { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
      type T        =             BigInt
      val  fullName = "scala.math.BigInt"

      override val parseString = BigInt.apply }

    // ---------------------------------------------------------------------------
    case object _BigDec extends UnparameterizedBasicType with UnboundedNumber { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
      type T        =             BigDecimal
      val  fullName = "scala.math.BigDecimal"

      override val parseString = BigDecimal.apply

      override protected def accessorNameModifier(value: FullName): String = value.replace("imal", "") }

    // ===========================================================================
    case object _LocalDate extends UnparameterizedBasicType with HasPair { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
        type T        =            LocalDate
        val  fullName = "java.time.LocalDate"

        override val pair = (_.parseLocalDate, _.toLocalDate /* aptus' */)

        private implicit val ord: Ordering[T] = CustomOrdering.localDate }

      // ---------------------------------------------------------------------------
      case object _LocalTime extends UnparameterizedBasicType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
        type T        =            LocalTime
        val  fullName = "java.time.LocalTime"

        override val parseString = _.parseLocalTime /* aptus' */

        private implicit val ord: Ordering[T] = CustomOrdering.localTime }
    
    // ===========================================================================
    case object _Instant extends UnparameterizedBasicType with HasPair { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
        type T        =            Instant
        val  fullName = "java.time.Instant"

        override val pair = (_.parseInstant, _.toInstant /* aptus' */)

        private implicit val ord: Ordering[T] = Ordering.by(identity) /* not sure why needed */ }
  
      // ---------------------------------------------------------------------------
      case object _OffsetDateTime extends UnparameterizedBasicType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
        type T        =            OffsetDateTime
        val  fullName = "java.time.OffsetDateTime"

        override val parseString = _.parseOffsetDateTime /* aptus' */

        private implicit val ord: Ordering[T] = CustomOrdering.offsetDateTime }
    
      // ---------------------------------------------------------------------------
      case object _ZonedDateTime extends UnparameterizedBasicType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
        type T        =            ZonedDateTime
        val  fullName = "java.time.ZonedDateTime"

        override val parseString = _.parseZonedDateTime  /* aptus' */

        private implicit val ord: Ordering[T] = CustomOrdering.zonedDateTime }
    
      // ---------------------------------------------------------------------------
      case object _LocalDateTime extends UnparameterizedBasicType with HasPair { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        type T        =            LocalDateTime
        val  fullName = "java.time.LocalDateTime"

        override val valuePredicate = _.isInstanceOf[T]
        override val pair           = (_.replace(" ", "T").parseLocalDateTime, _.toLocalDateTime /* aptus' */) // see https://stackoverflow.com/questions/9531524/in-an-iso-8601-date-is-the-t-character-mandatory

        private implicit val ord: Ordering[T] = CustomOrdering.localDateTime }

    // ===========================================================================
    case object _Binary extends UnparameterizedBasicType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; override val valuePredicate = _.isInstanceOf[T]
      type T        =           ByteBuffer
      val  fullName = "java.nio.ByteBuffer"

      override val parseString = data.DataFormatting.parseBinaryString

      private implicit val ord: Ordering[T] = CustomOrdering.byteBuffer }

    // ===========================================================================
    // TODO:
    // - t210330102827 - capture enum name for macros (currently stored in Fld hackily)
    case class _Enm(values: Seq[EnumValue]) extends ParameterizedBasicType { /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
        vldt.MetaValidation.checkAreValidEnumValues(values).require(_.isEmpty /* ie no errors reported */)

        type T        =             Seq[EnumValue]
        val  fullName = gallia.reflect._EnumValue

        def stringValues: Seq[EnumStringValue] = values.map(_.stringValue)

        override val valuePredicate = _.isInstanceOf[EnumValue]

        // ---------------------------------------------------------------------------
        override protected def accessorNameModifier(value: FullName): String = "enm"
        private implicit val ord: Ordering[T] = CustomOrdering.enumValue }

      // ===========================================================================
      object _Enm extends HasFieldHasType with HasParseString {
        type T = EnumValue
        override def has         = _.hasEnum
        override def parseString = EnumValue.apply

        // ---------------------------------------------------------------------------
        private[gallia] val Dummy = _Enm(Seq(EnumValue("_"))) /* useful for internal comparisons in validation, see 220506101842 */ }

    // ===========================================================================
    /*
  		note: scala vs java:
    		assert(new java.lang.String("foo") == "foo")
    		assert(new java.lang.Boolean(true) == true)

        assert(new java.lang.Integer(1)   == 1)
        assert(new java.lang.Double (1.1) == 1.1)
            
        assert(new java.lang.Long (1)   == 1L)
        assert(new java.lang.Float(1.1) == 1.1F)
        
        assert(new java.lang.Byte (1: Byte)  == (1: Byte))
        assert(new java.lang.Short(1: Short) == (1: Short))
        
        // not directly equals
        assert(java.math.BigInteger.valueOf(1)   == BigInt    (1)  .bigInteger)
        assert(java.math.BigDecimal.valueOf(1.1) == BigDecimal(1.1).bigDecimal)    
    */
  }

// ===========================================================================
