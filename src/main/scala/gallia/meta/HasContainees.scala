package gallia
package meta

import aptus._

// ===========================================================================
trait HasValueTypes {  // see t210125111338 (union types)
  def valueTypes: Seq[ValueType]

  // ===========================================================================
  def  isNesting: Boolean = valueTypes.forall(_.isNesting)
  def hasNesting: Boolean = valueTypes.exists(_.isNesting)

  // ---------------------------------------------------------------------------
  def  isBasicType: Boolean = valueTypes.forall(_.isLeaf)
  def hasBasicType: Boolean = valueTypes.exists(_.isLeaf)

  // ===========================================================================
  def nestedClassOpt                                                 : Option[Cls] = valueTypes.flatMap(_.nestingOpt).force.option // TODO: handle unions
  def nestedClassesOpt(disambiguatorOpt: UnionObjectDisambiguatorOpt): Seq   [Cls] =
    valueTypes
      .flatMap(_.nestingOpt)
      .pipe { ncs => disambiguatorOpt.map(_.filter(ncs)).getOrElse(ncs) }

  // ---------------------------------------------------------------------------
  def forceNestedClass: Cls = nestedClassOpt.get
  def forceNestedClass(disambiguatorOpt: UnionObjectDisambiguatorOpt): Cls  = nestedClassesOpt(disambiguatorOpt).ifOneOrElse(errorMessage = _.map(_.nameOpt) -> disambiguatorOpt)

  // ---------------------------------------------------------------------------
  def basicTypeOpt  : Option[    BasicType ] = valueTypes.flatMap(_.leafOpt).force.option
  def basicTypesOpt : Option[Seq[BasicType]] = valueTypes.flatMap(_.leafOpt).inNoneIfEmpty
  def forceBasicType:            BasicType   = basicTypeOpt.get

  // ---------------------------------------------------------------------------
  def    isNumericalType   : Boolean               = basicTypeOpt.exists (_.isNumericalType)
  def      numericalTypeOpt: Option[NumericalType] = basicTypeOpt.flatMap(_.asNumericalTypeOpt)
  def forceNumericalType   :        NumericalType  = numericalTypeOpt.get

  // ---------------------------------------------------------------------------
  def    hasBasicType(value: BasicType): Boolean = valueTypes.exists(_.leafOpt == Some(value))
  def areAllBasicType(value: BasicType): Boolean = valueTypes.forall(_.leafOpt == Some(value)) // + only 1 by design (see a220411090125)

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

  def isBinary: Boolean = areAllBasicType(BasicType._Binary)

  def isEnum  : Boolean = valueTypes.forall(_.leafOpt.exists(_.isEnm))

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

  def hasBinary: Boolean = hasBasicType(BasicType._Binary)

  def hasEnum: Boolean = valueTypes.exists(_.leafOpt.exists(_.isEnm))

  // ===========================================================================
  def forceBasicTypes      : Seq[BasicType      ] = valueTypes.map(_.leafOpt.get)
  def forceNumericalTypes  : Seq[NumericalType  ] = valueTypes.map(_.leafOpt.get).flatMap(_.asNumericalTypeOpt)
  def forceUnboundedNumbers: Seq[UnboundedNumber] = valueTypes.map(_.leafOpt.get).flatMap(_.asUnboundedNumberOpt)
  def forceBoundedNumbers  : Seq[  BoundedNumber] = valueTypes.map(_.leafOpt.get).flatMap(_.asBoundedNumberOpt)
  def forceIntegerLikeTypes: Seq[IntegerLikeType] = valueTypes.map(_.leafOpt.get).flatMap(_.asIntegerLikeTypeOpt)
  def forceRealLikeTypes   : Seq[RealLikeType   ] = valueTypes.map(_.leafOpt.get).flatMap(_.asRealLikeTypeOpt)

  // ===========================================================================
  /** nesting to non-(necessarily-)nesting */
  protected def __lookup: Map[Index, Index] = {
    var nestingIndex = -1

    valueTypes
      .zipWithIndex
      .flatMap { case (subInfo, valueTypeIndex) =>
        if (subInfo.isNesting) { nestingIndex += 1; Some(nestingIndex -> valueTypeIndex) }
        else                                        None }
      .toMap }

}

// ===========================================================================
