package gallia
package meta

import aptus._

// ===========================================================================
trait HasContainees {  // see t210125111338 (union types)
  protected val _containees: Seq[Containee]

  // ===========================================================================
  def  isNesting: Boolean = _containees.forall(_.isNesting)
  def hasNesting: Boolean = _containees.exists(_.isNesting)

  // ---------------------------------------------------------------------------
  def  isBasicType: Boolean = _containees.forall(_.isLeaf)
  def hasBasicType: Boolean = _containees.exists(_.isLeaf)

  // ===========================================================================
  def nestedClassOpt  : Option[    Cls ] = _containees.flatMap(_.nestingOpt).force.option
  def nestedClassesOpt: Option[Seq[Cls]] = _containees.flatMap(_.nestingOpt).in.noneIf(_.isEmpty)

  // ---------------------------------------------------------------------------
  def forceNestedClass                     : Cls  = nestedClassOpt.get
  def forceNestedClass(nameOpt: ClsNameOpt): Cls =
    _containees
      .flatMap(_.nestingOpt)
      .filter { nc => nameOpt.forall { name => nc.nameOpt == Some(name) } }
      match {
        case Seq(sole) => sole
        case ncs       => aptus.illegalState(ncs.size, nameOpt, ncs.map(_.nameOpt)) }

  // ---------------------------------------------------------------------------
  def basicTypeOpt  : Option[BasicType] = _containees.flatMap(_.leafOpt).force.option
  def forceBasicType:        BasicType  = basicTypeOpt.get

  // ---------------------------------------------------------------------------
  def    isNumericalType   : Boolean               = basicTypeOpt.exists (_.isNumericalType)
  def      numericalTypeOpt: Option[NumericalType] = basicTypeOpt.flatMap(_.asNumericalTypeOpt)
  def forceNumericalType   :        NumericalType  = numericalTypeOpt.get

  // ---------------------------------------------------------------------------
  def    hasBasicType(value: BasicType): Boolean = _containees.exists(_.leafOpt == Some(value))
  def areAllBasicType(value: BasicType): Boolean = _containees.forall(_.leafOpt == Some(value)) // + only 1 by design (see a220411090125)

  // ---------------------------------------------------------------------------
  def isBoolean : Boolean = areAllBasicType(BasicType._Boolean)
  def isString  : Boolean = areAllBasicType(BasicType._String)
  def isInt     : Boolean = areAllBasicType(BasicType._Int)
  def isDouble  : Boolean = areAllBasicType(BasicType._Double)

  def isByte : Boolean = areAllBasicType(BasicType._Byte)
  def isLong : Boolean = areAllBasicType(BasicType._Long)
  def isShort: Boolean = areAllBasicType(BasicType._Short)
  def isFloat: Boolean = areAllBasicType(BasicType._Float)

  def isBigInt: Boolean = areAllBasicType(BasicType._BigInt)
  def isBigDec: Boolean = areAllBasicType(BasicType._BigDec)

  def isLocalDate     : Boolean = areAllBasicType(BasicType._LocalDate)
  def isLocalTime     : Boolean = areAllBasicType(BasicType._LocalTime)
  def isLocalDateTime : Boolean = areAllBasicType(BasicType._LocalDateTime)
  def isOffsetDateTime: Boolean = areAllBasicType(BasicType._OffsetDateTime)
  def isZonedDateTime : Boolean = areAllBasicType(BasicType._ZonedDateTime)
  def isInstant       : Boolean = areAllBasicType(BasicType._Instant)

  def isEnum : Boolean = areAllBasicType(BasicType._Enum)

  def isBinary: Boolean = areAllBasicType(BasicType._Binary)

  // ---------------------------------------------------------------------------
  def hasBoolean : Boolean = hasBasicType(BasicType._Boolean)
  def hasString  : Boolean = hasBasicType(BasicType._String)
  def hasInt     : Boolean = hasBasicType(BasicType._Int)
  def hasDouble  : Boolean = hasBasicType(BasicType._Double)

  def hasByte : Boolean = hasBasicType(BasicType._Byte)
  def hasLong : Boolean = hasBasicType(BasicType._Long)
  def hasShort: Boolean = hasBasicType(BasicType._Short)
  def hasFloat: Boolean = hasBasicType(BasicType._Float)

  def hasBigInt: Boolean = hasBasicType(BasicType._BigInt)
  def hasBigDec: Boolean = hasBasicType(BasicType._BigDec)

  def hasLocalDate     : Boolean = hasBasicType(BasicType._LocalDate)
  def hasLocalTime     : Boolean = hasBasicType(BasicType._LocalTime)
  def hasLocalDateTime : Boolean = hasBasicType(BasicType._LocalDateTime)
  def hasOffsetDateTime: Boolean = hasBasicType(BasicType._OffsetDateTime)
  def hasZonedDateTime : Boolean = hasBasicType(BasicType._ZonedDateTime)
  def hasInstant       : Boolean = hasBasicType(BasicType._Instant)

  def hasEnum : Boolean = hasBasicType(BasicType._Enum)

  def hasBinary: Boolean = hasBasicType(BasicType._Binary)

  // ===========================================================================
  def forceBasicTypes      : Seq[BasicType      ] = _containees.map(_.leafOpt.get)
  def forceNumericalTypes  : Seq[NumericalType  ] = _containees.map(_.leafOpt.get).flatMap(_.asNumericalTypeOpt)
  def forceUnboundedNumbers: Seq[UnboundedNumber] = _containees.map(_.leafOpt.get).flatMap(_.asUnboundedNumberOpt)
  def forceBoundedNumbers  : Seq[  BoundedNumber] = _containees.map(_.leafOpt.get).flatMap(_.asBoundedNumberOpt)
  def forceIntegerLikeTypes: Seq[IntegerLikeType] = _containees.map(_.leafOpt.get).flatMap(_.asIntegerLikeTypeOpt)
  def forceRealLikeTypes   : Seq[RealLikeType   ] = _containees.map(_.leafOpt.get).flatMap(_.asRealLikeTypeOpt)
}

// ===========================================================================
