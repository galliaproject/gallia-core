package gallia
package meta
package basic

import aptus.String_
import atoms.utils.SuperMetaPair

// ===========================================================================
private[gallia] trait BasicTypeHelper { ignored: BasicType =>
  import OptionOrdering._

  // ---------------------------------------------------------------------------
  final     def accessorName                       : String = fullName.pipe(accessorNameModifier).splitBy(".").last.uncapitalizeFirst
  protected def accessorNameModifier(value: String): String = value // overriden by some: BigDec, Enum, ...

  // ===========================================================================
  def superPair(container: Container, descending: Boolean, missingLast: Boolean) =
      _superPair(container, descending, missingLast)

  // ---------------------------------------------------------------------------
  def compare(container: Container, descending: Boolean, missingLast: Boolean)(x: AnyValue, y: AnyValue): Int =
      _compare(container, descending, missingLast)(x, y)

  // ===========================================================================
  def _ctag: ClassTag[                T ]
  def nctag: ClassTag[       Iterable[T] ]
  def octag: ClassTag[       Option  [T] ]
  def pctag: ClassTag[Option[Iterable[T]]]

  // ===========================================================================
  def  ordA: Ordering[                 T ]
  def  ordD: Ordering[                 T ]

  def nordA: Ordering[       Iterable [T]] = Ordering.Iterable(ordA)
  def nordD: Ordering[       Iterable [T]] = Ordering.Iterable(ordD)

  def oordAF: Ordering[       Option  [T]] = optionAF(ordA)
  def oordAL: Ordering[       Option  [T]] = optionAL(ordA)
  def oordDF: Ordering[       Option  [T]] = optionDF(ordD)
  def oordDL: Ordering[       Option  [T]] = optionDL(ordD)

  def pordAF: Ordering[Option[Iterable[T]]] = optionAF(nordA)
  def pordAL: Ordering[Option[Iterable[T]]] = optionAL(nordA)
  def pordDF: Ordering[Option[Iterable[T]]] = optionDF(nordD)
  def pordDL: Ordering[Option[Iterable[T]]] = optionDL(nordD)

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
        case Container._One => SuperMetaPair(_ctag, (if (descending)  ordD else  ordA))
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
