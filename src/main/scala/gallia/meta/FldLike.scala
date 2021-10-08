package gallia.meta

import aptus.Anything_
import aptus.Option_
import gallia._
import gallia.reflect.Container._

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
      @gallia.PartialTypeMatching
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

        def isString  : Boolean = isBasicType(BasicType._String)

        def isInt     : Boolean = isBasicType(BasicType._Int)
        def isDouble  : Boolean = isBasicType(BasicType._Double)

        def isBoolean : Boolean = isBasicType(BasicType._Boolean)

        // ---------------------------------------------------------------------------
        def forceBasicType: BasicType = _containee.leafOpt.force
        def forceNumericalType: NumericalType = numericalTypeOpt.force

        // ---------------------------------------------------------------------------
        @gallia.NumberAbstraction
        def numericalTypeOpt: Option[NumericalType] =
               if (isInt)    BasicType._Int   .in.some
          else if (isDouble) BasicType._Double.in.some
          else               None

        // ---------------------------------------------------------------------------
        def isNumerical : Boolean = isInt || isDouble // eg for sum-by - FIXME: more allowed types
    }

// ===========================================================================
