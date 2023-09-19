package gallia
package meta

import aptus._
import GalliaUtils.Seq__

// ===========================================================================
case class Info(
       optional: Optional,
       union   : Seq[SubInfo] /* see https://github.com/galliaproject/gallia-docs/blob/master/union_types.md */)
      extends InfoLike {

    override protected val _info: Info = this

    // ---------------------------------------------------------------------------
    union // see https://github.com/galliaproject/gallia-docs/blob/master/union_types.md
      .requireNonEmpty
      .requireDistinct

    // ===========================================================================
    override def toString = formatDefault
      def formatDefault: String =
        //  see t210125111338 (union types)
        if (optional) s"${formatOptional(optional)}\t${union.map(_.formatDefault).join("|")}"
        else                                           union.map(_.formatDefault).join("|")

    // ===========================================================================
    private def updateSubInfos(newSubInfos: Seq[SubInfo]) = copy(union = newSubInfos.distinct)

      def toSingle  : Info = updateSubInfos(newSubInfos = union.map(_.toSingle))
      def toMultiple: Info = updateSubInfos(newSubInfos = union.map(_.toMultiple))

      def toRequired: Info = copy(optional = false)
      def toOptional: Info = copy(optional = true)

    // ===========================================================================
    def transformSoleSubInfo                               (f: SubInfo => SubInfo)    : Info = updateSubInfos(newSubInfos = f(subInfo1))
    def transformAllSubInfos                               (f: SubInfo => SubInfo)    : Info = updateSubInfos(newSubInfos = union.map(f))
    def transformSpecificSubInfo  (p: SubInfo   => Boolean)(f: SubInfo => SubInfo)    : Info = updateSubInfos(newSubInfos = union.mapAffectExactlyOne(p)(f)) // see t210125111338 (union types)
    def transformSpecificValueType(p: ValueType => Boolean)(f: ValueType => ValueType): Info = updateSubInfos(newSubInfos = union.mapAffectExactlyOne(_.valueType.pipe(p))(_.transformValueType(f)))

    // ---------------------------------------------------------------------------
    def transformNestedClasses                    (f: Cls  => Cls): Info = updateSubInfos(newSubInfos = union.mapIf              (_.isNesting)                     (_.transformNestedClass(f)))
    def transformSoleNestedClass                  (f: Cls  => Cls): Info = updateSubInfos(newSubInfos = union.mapAffectExactlyOne(_.isNesting)                     (_.transformNestedClass(f)))
    def transformNestedClass(target: Cls)         (f: Cls  => Cls): Info = updateSubInfos(newSubInfos = union.mapAffectExactlyOne(_.nestedClassOpt == Some(target))(_.transformNestedClass(f)))
    def transformNestedClass(target: Index)       (f: Cls  => Cls): Info = updateSubInfos(newSubInfos = union.mapIndex           (__lookup(target))                (_.transformNestedClass(f)))
    def transformNestedClass(pred: Cls => Boolean)(f: Cls  => Cls): Info = updateSubInfos(newSubInfos = union.mapAffectExactlyOne(_.nestedClassOpt.exists(pred))   (_.transformNestedClass(f)))

    def transformNestedClass(target: UnionObjectDisambiguator)(f: Cls  => Cls): Info = target match {
        case    DisambiguateByClassIndex(index) => transformNestedClass(index)(f)
        case x: DisambiguateByClassPredicate    => transformNestedClass(x.meta) (f) }

    // ---------------------------------------------------------------------------
    def updateSoleSubInfo                       (newValue: SubInfo): Info = transformSoleSubInfo                   (_ => newValue)
    def updateSpecificSubInfo(oldValue: SubInfo, newValue: SubInfo): Info = transformSpecificSubInfo(_ == oldValue)(_ => newValue) // see t210125111338 (union types)

    // ---------------------------------------------------------------------------
    def updateValueType  (valueType: ValueType): Info = transformSoleSubInfo(_.updateValueType(valueType))
    def updateOptionality(value: Optional)     : Info = copy(optional = value)

    // ===========================================================================
    def potentiallyProcessNesting(value: AnyValue): AnyValue =
      nestedClassOpt
        .map { nestedClass =>
          container1.containerWrap(target.Instantiator.valueToObj(nestedClass))(value) }
        .getOrElse(value)
  }

  // ===========================================================================
  object Info {
    def required(subInfo: SubInfo) = Info(_Required, Seq(subInfo))
    def optional(subInfo: SubInfo) = Info(_Optional, Seq(subInfo))

    // ---------------------------------------------------------------------------
    def one(valueType: ValueType): Info = Info(_Required, Seq(SubInfo(_Single  , valueType)))
    def opt(valueType: ValueType): Info = Info(_Optional, Seq(SubInfo(_Single, valueType)))
    def nes(valueType: ValueType): Info = Info(_Required, Seq(SubInfo(_Multiple, valueType)))
    def pes(valueType: ValueType): Info = Info(_Optional, Seq(SubInfo(_Multiple, valueType)))

    // ---------------------------------------------------------------------------
    def one(basic: BasicType.type => BasicType): Info = Info(_Required, Seq(SubInfo(_Single  , basic(BasicType))))
    def opt(basic: BasicType.type => BasicType): Info = Info(_Optional, Seq(SubInfo(_Single  , basic(BasicType))))
    def nes(basic: BasicType.type => BasicType): Info = Info(_Required, Seq(SubInfo(_Multiple, basic(BasicType))))
    def pes(basic: BasicType.type => BasicType): Info = Info(_Optional, Seq(SubInfo(_Multiple, basic(BasicType))))

    // ---------------------------------------------------------------------------
    def oneBoolean: Info = one(_._Boolean)
    def oneInt    : Info = one(_._Int)
    def oneDouble : Info = one(_._Double)
    def oneString : Info = one(_._String)

    // ---------------------------------------------------------------------------
    def optBoolean: Info = opt(_._Boolean)
    def optInt    : Info = opt(_._Int)
    def optDouble : Info = opt(_._Double)
    def optString : Info = opt(_._String)

    // ===========================================================================
    def forceFrom[T : WTT]: Info = TypeNode.parse[T].forceNonBObjInfo

    // ===========================================================================
    def combine(left: Info, right: Info): Info = { import gallia.vldt.SpecialCardiMode
      require(
        vldt.MetaValidationCompatibility.compatible(left, right, SpecialCardiMode.Normal),
        (left, right))

      val combinedContainer = Container.combine(left.container1, right.container1)

      Info(
        optional = combinedContainer.isOptional,
        union    = Seq(SubInfo(
          combinedContainer.isMultiple,
          ValueType.combine(left.subInfo1.valueType, right.subInfo1.valueType))))
    }
  }

// ===========================================================================
