package gallia
package reflect

import scala.reflect.{classTag, ClassTag}
import enumeratum.{Enum, EnumEntry}
import java.time._

import aptus.{Anything_, Seq_, String_}

// ===========================================================================
sealed trait NumericalType extends BasicType

  // ---------------------------------------------------------------------------
  sealed trait UnboundedNumber extends NumericalType // bignums
  sealed trait   BoundedNumber extends NumericalType // excluding bignums
  
    // ---------------------------------------------------------------------------
    sealed trait IntegerLikeType extends BoundedNumber {
        // especially useful for JSON (since doesn't differentiate integers)
        def toIntegerLike(value: Double): Any /* eg value.toLong for _Long */ }    

    // ---------------------------------------------------------------------------
    sealed trait RealLikeType extends BoundedNumber {
        // especially useful for JSON (since doesn't differentiate integers)
        def toRealLike(value: Double): Any /* eg value.toDouble for _Float */ }    

// ===========================================================================
// note: intentionally not called "PrimitiveType"
sealed trait BasicType // TODO: t210125111338 - investigate union types (coming in scala 3?)
      extends EnumEntry
      with    BasicTypeHelper
      with    meta.Containee {
    type T

    // ---------------------------------------------------------------------------
    val fullName: FullName

      final     def accessorName: String = fullName.pipe(accessorNameModifier).splitBy(".").last.uncapitalizeFirst
      protected def accessorNameModifier(value: FullName): String = value // overriden by some: BigDec, Enum, ...
    
    // ---------------------------------------------------------------------------
    def isInt    : Boolean = this == BasicType._Int
    def isDouble : Boolean = this == BasicType._Double
    def isBoolean: Boolean = this == BasicType._Boolean
    def isString : Boolean = this == BasicType._String
    
    def isByte : Boolean = this == BasicType._Byte
    def isShort: Boolean = this == BasicType._Short
    def isLong : Boolean = this == BasicType._Long
    def isFloat: Boolean = this == BasicType._Float

    // ---------------------------------------------------------------------------    
    def isNumericalType  : Boolean = this.isInstanceOf[NumericalType]
    def isIntegerLikeType: Boolean = this.isInstanceOf[IntegerLikeType]
    def isRealLikeType   : Boolean = this.isInstanceOf[RealLikeType]
    
    def asNumericalTypeOpt   = if (this.isInstanceOf[NumericalType])   Some(this.asInstanceOf[NumericalType])   else None
    def asIntegerLikeTypeOpt = if (this.isInstanceOf[IntegerLikeType]) Some(this.asInstanceOf[IntegerLikeType]) else None
    def asRealLikeTypeOpt    = if (this.isInstanceOf[RealLikeType])    Some(this.asInstanceOf[RealLikeType])    else None

    // ===========================================================================
    final lazy val alias: Option[Alias] = ReflectUtils.simplify(fullName).in.noneIf(_ == fullName)

    final lazy val node: TypeNode = TypeNode(TypeLeaf(fullName, fullName.split("\\.").last, alias) , Nil)

    // ===========================================================================
    def superPair(container: Container, descending: Boolean, missingLast: Boolean) =
        _superPair(container, descending, missingLast)

    // ---------------------------------------------------------------------------
    def compare(container: Container, descending: Boolean, missingLast: Boolean)(x: AnyValue, y: AnyValue): Int =
        _compare(container, descending, missingLast)(x, y)

    // ===========================================================================
    val  ctag: ClassTag[                T ]
    val nctag: ClassTag[       Iterable[T] ]
    val octag: ClassTag[       Option  [T] ]
    val pctag: ClassTag[Option[Iterable[T]]]

    // ===========================================================================
    import OptionOrdering._

         val  ordA: Ordering[                 T ]
         val  ordD: Ordering[                 T ]

    lazy val nordA: Ordering[       Iterable [T]] = Ordering.Iterable(ordA)
    lazy val nordD: Ordering[       Iterable [T]] = Ordering.Iterable(ordD)

    lazy val oordAF: Ordering[       Option  [T]] = optionAF(ordA)
    lazy val oordAL: Ordering[       Option  [T]] = optionAL(ordA)
    lazy val oordDF: Ordering[       Option  [T]] = optionDF(ordD)
    lazy val oordDL: Ordering[       Option  [T]] = optionDL(ordD)

    lazy val pordAF: Ordering[Option[Iterable[T]]] = optionAF(nordA)
    lazy val pordAL: Ordering[Option[Iterable[T]]] = optionAL(nordA)
    lazy val pordDF: Ordering[Option[Iterable[T]]] = optionDF(nordD)
    lazy val pordDL: Ordering[Option[Iterable[T]]] = optionDL(nordD)
  }

  // ===========================================================================
  @TypeMatching object BasicType extends Enum[BasicType] {
    val values = findValues

    // ===========================================================================
    def fromFullNameOpt(value: FullName): Option[BasicType] = _lookup.get     (normalize(value))
    def fromFullName   (value: FullName):        BasicType  = _lookup.apply   (normalize(value))
    def isKnown        (value: FullName):        Boolean    = _lookup.contains(normalize(value))
    
    // ---------------------------------------------------------------------------
      private val _lookup: Map[FullName, BasicType] = values.map { x => x.fullName -> x }.force.map

      private def lookup(value: FullName): BasicType = _lookup.getOrElse(
          value,
          aptus.illegalState(s"TODO:CantFindType:201013093225:${value}"))

      // ---------------------------------------------------------------------------
      private def normalize(value: FullName): FullName =
        if (value == "java.lang.String") value
        else                             value.replace("java.lang.", "scala.") // not so for java.math (not equivalent at runtime)

    // ===========================================================================
    def combine(values: Seq[BasicType]): BasicType = // TODO: subtype these 3?
      values.distinct.sortBy(_.entryName) match {
        case Seq(                   BasicType._Int                   ) => BasicType._Int
        case Seq(BasicType._Double                                   ) => BasicType._Double
        case Seq(BasicType._Double, BasicType._Int                   ) => BasicType._Double
        case Seq(                                   BasicType._String) => BasicType._String
        case Seq(_                                , BasicType._String) => BasicType._String
        case Seq(_                , _             , BasicType._String) => BasicType._String }

    // ===========================================================================
    // note: excluding Char and Unit intentionally

    // TODO:
    // - t210108114447 - support own "flag" type?
    // - t210109142406 - dedicated matrix/tensor object (dense/sparse); look into existing libraries
    // - t210110094829 - accept Obj as value, albeit the standalone version (see t210104164037)
    // - t210110095252 - BLOB/CLOB (for now must use base64ed version)
    // - change names upon serialization (eg "string" instead of _String)

    case object _String  extends BasicType {
      type T = String
      val fullName = "java.lang.String"

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

    // ---------------------------------------------------------------------------
    case object _Boolean extends BasicType     { type T = Boolean; val fullName = "scala.Boolean"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // TODO: keep? should only be use as option to emulate flag...
    //case object _Unit extends BasicType { type T = Unit; val fullName = "scala.Unit"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // ---------------------------------------------------------------------------
    case object _Int     extends IntegerLikeType { type T = Int    ; val fullName = "scala.Int"   ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toInt  }
    case object _Double  extends RealLikeType    { type T = Double ; val fullName = "scala.Double"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toRealLike   (value: Double) = value }

    //TODO: rename these to Int{8, 16, 64}?
    case object _Byte  extends IntegerLikeType   { type T = Byte ; val fullName = "scala.Byte" ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toByte  }
    case object _Short extends IntegerLikeType   { type T = Short; val fullName = "scala.Short"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toShort }
    case object _Long  extends IntegerLikeType   { type T = Long ; val fullName = "scala.Long" ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toIntegerLike(value: Double) = value.toLong  }

    case object _Float extends RealLikeType      { type T = Float ; val fullName = "scala.Float"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; def toRealLike   (value: Double) = value.toFloat }

    // ---------------------------------------------------------------------------
    case object _BigInt extends UnboundedNumber { type T = BigInt    ; val fullName = "scala.math.BigInt"    ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse; }
    case object _BigDec extends UnboundedNumber { type T = BigDecimal; val fullName = "scala.math.BigDecimal"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse
      override protected def accessorNameModifier(value: FullName): String = value.replace("imal", "") }

    // ===========================================================================
    case object _LocalDate     extends BasicType { type T = LocalDate    ; val fullName = "java.time.LocalDate"
        private implicit val ord: Ordering[T] = CustomOrdering.localDate
          /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

      // ---------------------------------------------------------------------------
      case object _LocalTime extends BasicType { type T = LocalTime; val fullName = "java.time.LocalTime"
        private implicit val ord: Ordering[T] = CustomOrdering.localTime
          /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    
    // ===========================================================================
    case object _Instant extends BasicType { type T = LocalDateTime; val fullName = "java.time.Instant"
        private implicit val ord: Ordering[T] = Ordering.by(identity) // not sure why needed 
          /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
  
      // ---------------------------------------------------------------------------
      case object _OffsetDateTime extends BasicType { type T = OffsetDateTime; val fullName = "java.time.OffsetDateTime"
        private implicit val ord: Ordering[T] = CustomOrdering.offsetDateTime
          /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    
      // ---------------------------------------------------------------------------
      case object _ZonedDateTime extends BasicType { type T = ZonedDateTime; val fullName = "java.time.ZonedDateTime"
        private implicit val ord: Ordering[T] = CustomOrdering.zonedDateTime
          /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    
      // ---------------------------------------------------------------------------
      case object _LocalDateTime extends BasicType { type T = LocalDateTime; val fullName = "java.time.LocalDateTime"
        private implicit val ord: Ordering[T] = CustomOrdering.localDateTime
          /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // ===========================================================================
    // TODO:
    // - t210114093525 - capture enum values (will need to adapt serialization)
    // - t210330102827 - capture enum name for macros
    case object _Enum extends BasicType { type T = EnumEntry; val fullName = "enumeratum.EnumEntry"
    		override protected def accessorNameModifier(value: FullName): String = value.replace("EnumEntry",  "enm") // "enum" is reserved in Scala 3      
      private implicit val ord: Ordering[T] = CustomOrdering.enumEntry
      /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // ===========================================================================
    case object _Binary extends BasicType { type T = ByteBuffer; val fullName = "java.nio.ByteBuffer"
      private implicit val ord: Ordering[T] = CustomOrdering.byteBuffer
        /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

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
