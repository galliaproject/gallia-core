package gallia.reflect

import scala.reflect.{classTag, ClassTag}
import enumeratum.{Enum, EnumEntry}
import java.time._

import aptus.{Anything_, Seq_}

import gallia.AnyValue
import gallia.meta.Containee

// ===========================================================================
sealed trait NumericalType extends BasicType

// ===========================================================================
sealed trait BasicType // TODO: t210125111338 - investigate union types (coming in scala 3?)
      extends EnumEntry
      with    BasicTypeHelper
      with    Containee {
    type T

    val fullName: FullName

    // ---------------------------------------------------------------------------
    def isInt    : Boolean = this == BasicType._Int
    def isDouble : Boolean = this == BasicType._Double
    def isBoolean: Boolean = this == BasicType._Boolean
    def isString : Boolean = this == BasicType._String

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
  @gallia.TypeMatching object BasicType extends Enum[BasicType] {
    val values = findValues

    // ---------------------------------------------------------------------------
    private val _lookup: Map[FullName, BasicType] = values.map { x => x.fullName -> x }.force.map

    private def lookup(value: FullName): BasicType = _lookup.getOrElse(
        value,
        aptus.illegalState(s"TODO:CantFindType:201013093225:${value}"))

    // ---------------------------------------------------------------------------
    def fromFullNameOpt(value: FullName): Option[BasicType] = _lookup.get(value)
    def fromFullName   (value: FullName): BasicType =  lookup(value)
    def isKnown        (name : FullName): Boolean   = _lookup.contains(name)

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
    case object _Int     extends NumericalType { type T = Int    ; val fullName = "scala.Int"   ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    case object _Double  extends NumericalType { type T = Double ; val fullName = "scala.Double"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    case object _Byte  extends NumericalType { type T = Byte ; val fullName = "scala.Byte" ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    case object _Short extends NumericalType { type T = Short; val fullName = "scala.Short"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    case object _Long  extends NumericalType { type T = Long ; val fullName = "scala.Long" ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    case object _Float extends NumericalType { type T = Float ; val fullName = "scala.Float"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // ---------------------------------------------------------------------------
    case object _BigInt        extends NumericalType { type T = BigInt    ; val fullName = "scala.math.BigInt"    ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
    case object _BigDecimal    extends NumericalType { type T = BigDecimal; val fullName = "scala.math.BigDecimal"; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // ---------------------------------------------------------------------------
    // TODO:
    // - t210114093525 - capture enum values (will need to adapt serialization)
    // - t210330102827 - capture enum name for macros
    case object _Enum extends BasicType { type T = EnumEntry; val fullName = "enumeratum.EnumEntry"
      private implicit val ord: Ordering[T] = Ordering.by(_.entryName)
      /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // TODO: t210110095228 - scala enum

    // ---------------------------------------------------------------------------
    case object _LocalDate     extends BasicType { type T = LocalDate    ; val fullName = "java.time.LocalDate"
        private implicit val ord: Ordering[T] = Ordering.by(_.toEpochDay)
        /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

      // ---------------------------------------------------------------------------
      case object _LocalDateTime extends BasicType { type T = LocalDateTime; val fullName = "java.time.LocalDateTime"
        private implicit val ord: Ordering[T] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))
        /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }

    // ---------------------------------------------------------------------------
    // - TODO: t210124153304 - add them all, including the .sql ones; automatically convert to scala counterpart?
    case object _JavaLong  extends BasicType { type T = Long ; val fullName = "java.lang.Long" ; /* boilerplate: */ override lazy val ctag: ClassTag[T] = classTag[T]; override lazy val nctag: ClassTag[Iterable[T]] = classTag[Iterable[T]]; override lazy val octag: ClassTag[Option [T]] = classTag[Option [T]]; override lazy val pctag: ClassTag[Option[Iterable[T]]] = classTag[Option[Iterable[T]]]; override lazy val ordA: Ordering[T] = implicitly[Ordering[T]]; override lazy val ordD: Ordering[T] = implicitly[Ordering[T]].reverse }
  }

// ===========================================================================
