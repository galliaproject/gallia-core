package gallia
package meta

import aptus.{Anything_, Option_}

import reflect.Container._

// ===========================================================================
trait FldLike extends HasKey with InfoLike

  // ===========================================================================
  trait HasKey {
    val  key: Key
    def skey: SKey = key.name
  }

  // ===========================================================================
  trait InfoLike extends HasContainer with HasContainee {
      def isNotNesting: Boolean = !isNesting
      def isNotLeaf   : Boolean = !isLeaf

      def isNotMultiple : Boolean = !isMultiple
      def isNotRequired : Boolean = !isRequired

      // ---------------------------------------------------------------------------
      def isType[T: WTT]: Boolean = isType(TypeNode.parse[T])

        def isType(node: TypeNode): Boolean =
          node
            .assert(!_.isContainedBObj) // has already been validated by here (see 201014103336)
            .forceNonBObjInfo
            .pipe { that =>
              (this._container, this._containee) ==
              (that. container, that. containee) }

      // ---------------------------------------------------------------------------
      @PartialTypeMatching
        def isOneString  : Boolean = isOne && isString
        def isOneInt     : Boolean = isOne && isInt
        def isOneDouble  : Boolean = isOne && isDouble
        def isOneBoolean : Boolean = isOne && isBoolean
        //TODO: get nested cls like

    }

    // ===========================================================================
    trait HasContainer {
      protected val _container: Container

      // ---------------------------------------------------------------------------
      def isContainer(value: Container): Boolean = _container == value

        def isOne: Boolean = _container == _One
        def isOpt: Boolean = _container == _Opt

        def isNes: Boolean = _container == _Nes
        def isPes: Boolean = _container == _Pes

        // ---------------------------------------------------------------------------
        def isMultiple    : Boolean = isNes || isPes
        def isRequired    : Boolean = isOne || isNes
        def isOptional    : Boolean = !isRequired
    }

    // ===========================================================================
    trait HasContainee {
      protected val _containee: Containee

      // ---------------------------------------------------------------------------
      def isNesting: Boolean = _containee.nestingOpt.nonEmpty
      def isLeaf   : Boolean = _containee.leafOpt   .nonEmpty

      // ---------------------------------------------------------------------------
      def isBasicType(value: BasicType): Boolean = _containee.leafOpt.exists(_ == value)

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
      def isBasicType      : Boolean = _containee.leafOpt.nonEmpty
      def isNumericalType  : Boolean = _containee.leafOpt.exists(_.isNumericalType)
      def isUnboundedNumber: Boolean = _containee.leafOpt.exists(_.isUnboundedNumber)
      def isBoundedNumber  : Boolean = _containee.leafOpt.exists(_.isBoundedNumber)
      def isIntegerLikeType: Boolean = _containee.leafOpt.exists(_.isIntegerLikeType)
      def isrealLikeType   : Boolean = _containee.leafOpt.exists(_.isRealLikeType)

        // ---------------------------------------------------------------------------
        def basicTypeOpt      : Option[BasicType]       = _containee.leafOpt
        def numericalTypeOpt  : Option[NumericalType]   = _containee.leafOpt.flatMap(_.asNumericalTypeOpt)
        def unboundedNumberOpt: Option[UnboundedNumber] = _containee.leafOpt.flatMap(_.asUnboundedNumberOpt)
        def   boundedNumberOpt: Option[  BoundedNumber] = _containee.leafOpt.flatMap(_.asBoundedNumberOpt)
        def integerLikeTypeOpt: Option[IntegerLikeType] = _containee.leafOpt.flatMap(_.asIntegerLikeTypeOpt)
        def realLikeTypeOpt   : Option[RealLikeType]    = _containee.leafOpt.flatMap(_.asRealLikeTypeOpt)

        // ---------------------------------------------------------------------------
        def forceBasicType      : BasicType       = basicTypeOpt      .force
        def forceNumericalType  : NumericalType   = numericalTypeOpt  .force
        def forceUnboundedNumber: UnboundedNumber = unboundedNumberOpt.force
        def forceBoundedNumber  :   BoundedNumber =   boundedNumberOpt.force
        def forceIntegerLikeType: IntegerLikeType = integerLikeTypeOpt.force
        def forceRealLikeType   : RealLikeType    = realLikeTypeOpt   .force
    }

// ===========================================================================
