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
      def isString  : Boolean = isBasicType(BasicType._String)

      def isInt     : Boolean = isBasicType(BasicType._Int)
      def isDouble  : Boolean = isBasicType(BasicType._Double)

      def isBoolean : Boolean = isBasicType(BasicType._Boolean)

      def isByte : Boolean = isBasicType(BasicType._Byte)
      def isLong : Boolean = isBasicType(BasicType._Long)
      def isShort: Boolean = isBasicType(BasicType._Short)
      def isFloat: Boolean = isBasicType(BasicType._Float)
      
      def isBigInt    : Boolean = isBasicType(BasicType._BigInt)
      def isBigDecimal: Boolean = isBasicType(BasicType._BigDecimal)

      def isDate    : Boolean = isBasicType(BasicType._LocalDate)
      def isDateTime: Boolean = isBasicType(BasicType._LocalDateTime)
      
      //TODO: isEnum

      // ===========================================================================
      def isStringDoubleBoolean: Boolean = isString || isDouble || isBoolean // useful for JSON
      
      // ---------------------------------------------------------------------------
      def isBasicType      : Boolean = _containee.leafOpt.nonEmpty
      def isNumericalType  : Boolean = _containee.leafOpt.exists(_.isNumericalType)
      def isIntegerLikeType: Boolean = _containee.leafOpt.exists(_.isIntegerLikeType)

        // ---------------------------------------------------------------------------
        def basicTypeOpt      : Option[BasicType]       = _containee.leafOpt
        def numericalTypeOpt  : Option[NumericalType]   = _containee.leafOpt.flatMap(_.asNumericalTypeOpt)
        def integerLikeTypeOpt: Option[IntegerLikeType] = _containee.leafOpt.flatMap(_.asIntegerLikeTypeOpt)

        // ---------------------------------------------------------------------------
        def forceBasicType      : BasicType       = basicTypeOpt      .force
        def forceNumericalType  : NumericalType   = numericalTypeOpt  .force
        def forceIntegerLikeType: IntegerLikeType = integerLikeTypeOpt.force
    }

// ===========================================================================
