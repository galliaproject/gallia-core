package gallia
package meta

import aptus.Option_

// ===========================================================================
trait HasContainee1 {
    protected val _containee1: Containee

    // ---------------------------------------------------------------------------
    def isNesting: Boolean = _containee1.nestingOpt.nonEmpty
    def isLeaf   : Boolean = _containee1.leafOpt   .nonEmpty

    def isNestingWithName(name: String): Boolean = _containee1.nestingOpt.exists(_.nameOpt == Some(name))

    // ---------------------------------------------------------------------------
    def isBasicType(value: BasicType): Boolean = _containee1.leafOpt.exists(_ == value)

    // ---------------------------------------------------------------------------
    def isBoolean : Boolean = isBasicType(BasicType._Boolean)
    def isString  : Boolean = isBasicType(BasicType._String)
    def isInt     : Boolean = isBasicType(BasicType._Int)
    def isDouble  : Boolean = isBasicType(BasicType._Double)

    def isByte : Boolean = isBasicType(BasicType._Byte)
    def isLong : Boolean = isBasicType(BasicType._Long)
    def isShort: Boolean = isBasicType(BasicType._Short)
    def isFloat: Boolean = isBasicType(BasicType._Float)

    def isBigInt: Boolean = isBasicType(BasicType._BigInt)
    def isBigDec: Boolean = isBasicType(BasicType._BigDec)

    def isLocalDate     : Boolean = isBasicType(BasicType._LocalDate)
    def isLocalTime     : Boolean = isBasicType(BasicType._LocalTime)
    def isLocalDateTime : Boolean = isBasicType(BasicType._LocalDateTime)
    def isOffsetDateTime: Boolean = isBasicType(BasicType._OffsetDateTime)
    def isZonedDateTime : Boolean = isBasicType(BasicType._ZonedDateTime)
    def isInstant       : Boolean = isBasicType(BasicType._Instant)

    def isEnum : Boolean = isBasicType(BasicType._Enum)

    def isBinary: Boolean = isBasicType(BasicType._Binary)

    // ===========================================================================
    def isBasicType      : Boolean = _containee1.leafOpt.nonEmpty
    def isNumericalType  : Boolean = _containee1.leafOpt.exists(_.isNumericalType)
    def isUnboundedNumber: Boolean = _containee1.leafOpt.exists(_.isUnboundedNumber)
    def isBoundedNumber  : Boolean = _containee1.leafOpt.exists(_.isBoundedNumber)
    def isIntegerLikeType: Boolean = _containee1.leafOpt.exists(_.isIntegerLikeType)
    def isRealLikeType   : Boolean = _containee1.leafOpt.exists(_.isRealLikeType)

      // ---------------------------------------------------------------------------
      def basicTypeOpt      : Option[BasicType]       = _containee1.leafOpt
      def numericalTypeOpt  : Option[NumericalType]   = _containee1.leafOpt.flatMap(_.asNumericalTypeOpt)
      def unboundedNumberOpt: Option[UnboundedNumber] = _containee1.leafOpt.flatMap(_.asUnboundedNumberOpt)
      def   boundedNumberOpt: Option[  BoundedNumber] = _containee1.leafOpt.flatMap(_.asBoundedNumberOpt)
      def integerLikeTypeOpt: Option[IntegerLikeType] = _containee1.leafOpt.flatMap(_.asIntegerLikeTypeOpt)
      def realLikeTypeOpt   : Option[RealLikeType]    = _containee1.leafOpt.flatMap(_.asRealLikeTypeOpt)

      // ---------------------------------------------------------------------------
      def forceBasicType      : BasicType       = basicTypeOpt      .force
      def forceNumericalType  : NumericalType   = numericalTypeOpt  .force
      def forceUnboundedNumber: UnboundedNumber = unboundedNumberOpt.force
      def forceBoundedNumber  :   BoundedNumber =   boundedNumberOpt.force
      def forceIntegerLikeType: IntegerLikeType = integerLikeTypeOpt.force
      def forceRealLikeType   : RealLikeType    = realLikeTypeOpt   .force
  }

  // ===========================================================================
  trait HasContainees {  // see t210125111338 (union types)
    protected val _containees: Seq[Containee]

    // ---------------------------------------------------------------------------
    def hasBasicType(value: BasicType): Boolean = _containees.exists(_.leafOpt == Some(value))
    def  isBasicType(value: BasicType): Boolean = _containees.forall(_.leafOpt == Some(value)) // + only 1 by design (see a220411090125)

    // ---------------------------------------------------------------------------
    def isBoolean : Boolean = isBasicType(BasicType._Boolean)
    def isString  : Boolean = isBasicType(BasicType._String)
    def isInt     : Boolean = isBasicType(BasicType._Int)
    def isDouble  : Boolean = isBasicType(BasicType._Double)

    def isByte : Boolean = isBasicType(BasicType._Byte)
    def isLong : Boolean = isBasicType(BasicType._Long)
    def isShort: Boolean = isBasicType(BasicType._Short)
    def isFloat: Boolean = isBasicType(BasicType._Float)

    def isBigInt: Boolean = isBasicType(BasicType._BigInt)
    def isBigDec: Boolean = isBasicType(BasicType._BigDec)

    def isLocalDate     : Boolean = isBasicType(BasicType._LocalDate)
    def isLocalTime     : Boolean = isBasicType(BasicType._LocalTime)
    def isLocalDateTime : Boolean = isBasicType(BasicType._LocalDateTime)
    def isOffsetDateTime: Boolean = isBasicType(BasicType._OffsetDateTime)
    def isZonedDateTime : Boolean = isBasicType(BasicType._ZonedDateTime)
    def isInstant       : Boolean = isBasicType(BasicType._Instant)

    def isEnum : Boolean = isBasicType(BasicType._Enum)

    def isBinary: Boolean = isBasicType(BasicType._Binary)

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
    // see t210125111338 (union types)
    def forceBasicTypes      : Seq[BasicType      ] = _containees.map(_.leafOpt.get)
    def forceNumericalTypes  : Seq[NumericalType  ] = _containees.map(_.leafOpt.get).flatMap(_.asNumericalTypeOpt)
    def forceUnboundedNumbers: Seq[UnboundedNumber] = _containees.map(_.leafOpt.get).flatMap(_.asUnboundedNumberOpt)
    def forceBoundedNumbers  : Seq[  BoundedNumber] = _containees.map(_.leafOpt.get).flatMap(_.asBoundedNumberOpt)
    def forceIntegerLikeTypes: Seq[IntegerLikeType] = _containees.map(_.leafOpt.get).flatMap(_.asIntegerLikeTypeOpt)
    def forceRealLikeTypes   : Seq[RealLikeType   ] = _containees.map(_.leafOpt.get).flatMap(_.asRealLikeTypeOpt)
  }

// ===========================================================================
