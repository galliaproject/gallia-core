package gallia
package meta

import aptus.{Anything_, Seq_}

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
