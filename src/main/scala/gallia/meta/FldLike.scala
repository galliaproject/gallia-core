package gallia
package meta

import aptus.{Anything_, Option_, Seq_}

import reflect.Container._

// ===========================================================================
trait FldLike extends HasKey with InfosLike

  // ===========================================================================
  trait HasKey {
    val  key: Key
    def skey: SKey = key.name
  }

  // ===========================================================================
  trait Info1Like extends HasContainer1 with HasContainee1 {
      def isType[T: WTT]: Boolean = isType(TypeNode.parse[T])

        def isType(node: TypeNode): Boolean =
          node
            .assert(!_.isContainedBObj) // has already been validated by here (see 201014103336)
            .forceNonBObjInfo
            .pipe { that =>
              (this._container1, this._containee1) ==
              (that. container , that. containee) }

      // ---------------------------------------------------------------------------
      @PartialTypeMatching
        def isOneString  : Boolean = isOne && isString
        def isOneInt     : Boolean = isOne && isInt
        def isOneDouble  : Boolean = isOne && isDouble
        def isOneBoolean : Boolean = isOne && isBoolean
        //TODO: get nested cls like

      // ===========================================================================
      def nestedClassOpt: Option[Cls] = _containee1.nestingOpt
  
      def forceNestedClass: Cls = _containee1.nestingOpt.get        
    }

  // ===========================================================================
  trait InfosLike extends HasContainers with HasContainees {

      @PartialTypeMatching
        def isOneString  : Boolean = isOne && isString
        def isOneInt     : Boolean = isOne && isInt
        def isOneDouble  : Boolean = isOne && isDouble
        def isOneBoolean : Boolean = isOne && isBoolean
        //TODO: get nested cls like

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
      def    isNumericalType   : Boolean               = basicTypeOpt.exists(_.isNumericalType)      
      def      numericalTypeOpt: Option[NumericalType] = basicTypeOpt.flatMap(_.asNumericalTypeOpt)
      def forceNumericalType   :        NumericalType  = numericalTypeOpt.get
    }

    // ===========================================================================
    trait HasContainer1 {
      protected val _container1: Container

      // ---------------------------------------------------------------------------      
      def isContainer(value: Container): Boolean = _container1 == value

        def isOne: Boolean = isContainer(_One)
        def isOpt: Boolean = isContainer(_Opt)

        def isNes: Boolean = isContainer(_Nes)
        def isPes: Boolean = isContainer(_Pes)

        // ---------------------------------------------------------------------------
        // see t210125111338 (union types)
        def isSingle  : Boolean = _container1.isSingle
        def isMultiple: Boolean = _container1.isMultiple        
        def isRequired: Boolean = _container1.isRequired
        def isOptional: Boolean = _container1.isOptional
    }
    
    // ===========================================================================
    trait HasContainers {
      protected val _containers: Seq[Container] // see t210125111338 (union types)

      // ---------------------------------------------------------------------------      
      def isContainer(value: Container): Boolean = _containers.forall(_ == value) // see t210125111338 (union types)

        def isOne: Boolean = isContainer(_One)
        def isOpt: Boolean = isContainer(_Opt)

        def isNes: Boolean = isContainer(_Nes)
        def isPes: Boolean = isContainer(_Pes)

        // ---------------------------------------------------------------------------
        // see t210125111338 (union types)
        def isSingle  : Boolean = _containers.forall(_.isSingle)
        def isMultiple: Boolean = _containers.forall(_.isMultiple)        
        def isRequired: Boolean = _containers.forall(_.isRequired)
        def isOptional: Boolean = _containers.forall(_.isOptional)        
    }    

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
