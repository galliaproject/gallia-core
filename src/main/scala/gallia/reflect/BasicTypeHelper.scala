package gallia
package reflect

import scala.reflect.ClassTag
import aptus.{Anything_, String_}
import atoms.utils.SuperMetaPair

// ===========================================================================
private[reflect] trait BasicTypeHelper { ignored: BasicType =>
  import OptionOrdering._

  // ---------------------------------------------------------------------------
  final     def accessorName                         : String = fullName.pipe(accessorNameModifier).splitBy(".").last.uncapitalizeFirst
  protected def accessorNameModifier(value: FullName): String = value // overriden by some: BigDec, Enum, ...

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

  // ===========================================================================
  def isInt    : Boolean = this == BasicType._Int
  def isDouble : Boolean = this == BasicType._Double
  def isBoolean: Boolean = this == BasicType._Boolean
  def isString : Boolean = this == BasicType._String
  def isEnm    : Boolean = this.isInstanceOf[_Enm]

  // ---------------------------------------------------------------------------
  def isNumericalType  : Boolean = this.isInstanceOf[NumericalType]
  def isUnboundedNumber: Boolean = this.isInstanceOf[UnboundedNumber]
  def isBoundedNumber  : Boolean = this.isInstanceOf[BoundedNumber]
  def isIntegerLikeType: Boolean = this.isInstanceOf[IntegerLikeType]
  def isRealLikeType   : Boolean = this.isInstanceOf[RealLikeType]

  def forceNumericalType   = this.asInstanceOf[NumericalType]
  def forceUnboundedNumber = this.asInstanceOf[UnboundedNumber]
  def forceBoundedNumber   = this.asInstanceOf[BoundedNumber]
  def forceIntegerLikeType = this.asInstanceOf[IntegerLikeType]
  def forceRealLikeType    = this.asInstanceOf[RealLikeType]

  def asNumericalTypeOpt   = if (this.isInstanceOf[NumericalType])   Some(this.asInstanceOf[NumericalType])   else None
  def asUnboundedNumberOpt = if (this.isInstanceOf[UnboundedNumber]) Some(this.asInstanceOf[UnboundedNumber]) else None
  def asBoundedNumberOpt   = if (this.isInstanceOf[BoundedNumber])   Some(this.asInstanceOf[BoundedNumber])   else None
  def asIntegerLikeTypeOpt = if (this.isInstanceOf[IntegerLikeType]) Some(this.asInstanceOf[IntegerLikeType]) else None
  def asRealLikeTypeOpt    = if (this.isInstanceOf[RealLikeType])    Some(this.asInstanceOf[RealLikeType])    else None

  // ---------------------------------------------------------------------------
  def forceEnm:        _Enm  = enmOpt.get
  def enmOpt  : Option[_Enm] = if (isEnm) Some(this.asInstanceOf[BasicType._Enm]) else None

  // ===========================================================================
  def _superPair(container: Container, descending: Boolean, missingLast: Boolean) =
      container match {
        case Container._One => SuperMetaPair( ctag, (if (descending)  ordD else  ordA))
        case Container._Nes => SuperMetaPair(nctag, (if (descending) nordD else nordA))
        case Container._Opt => SuperMetaPair(octag,
          ((descending, missingLast) match {
            case (true , true ) => oordDL
            case (true , false) => oordDF
            case (false, true ) => oordAL
            case (false, false) => oordAF }))
        case Container._Pes => SuperMetaPair(pctag,
          ((descending, missingLast) match {
            case (true , true ) => pordDL
            case (true , false) => pordDF
            case (false, true ) => pordAL
            case (false, false) => pordAF })) }

  // ===========================================================================
  def _compare(container: Container, descending: Boolean, missingLast: Boolean)(x: AnyValue, y: AnyValue): Int =
    container match {
      case Container._One => (if (descending)  ordD else  ordA).compare(x.asInstanceOf[         T ], y.asInstanceOf[         T ])
      case Container._Nes => (if (descending) nordD else nordA).compare(x.asInstanceOf[Iterable[T]], y.asInstanceOf[Iterable[T]])

      // ---------------------------------------------------------------------------
      case Container._Opt => ((descending, missingLast) match {
          case (true , true ) => oordDL
          case (true , false) => oordDF
          case (false, true ) => oordAL
          case (false, false) => oordAF })
        .compare(x.asInstanceOf[Option[T]], y.asInstanceOf[Option[T]])
      // ---------------------------------------------------------------------------
      case Container._Pes => ((descending, missingLast) match {
          case (true , true ) => pordDL
          case (true , false) => pordDF
          case (false, true ) => pordAL
          case (false, false) => pordAF })
        .compare(x.asInstanceOf[Option[Iterable[T]]], y.asInstanceOf[Option[Iterable[T]]]) }

}

// ===========================================================================
