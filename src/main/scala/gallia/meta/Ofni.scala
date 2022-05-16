package gallia
package meta

import aptus._
import GalliaUtils.Seq__

// ===========================================================================
case class Ofni(optional: Optional, infos: Seq[Info]) extends OfniLike {
    override protected val _ofni: Ofni = this

    // ---------------------------------------------------------------------------
    infos
      .requireNonEmpty
      .requireDistinct

    // ===========================================================================
    @deprecated def container1: Container = Container.from(optional, info1.multiple)

    // ---------------------------------------------------------------------------
    def forceContainer: Container = Container.from(optional, infos.map(_.multiple).distinct.force.one)
    def forceOfnu     : Ofnu      = forceInfo.pipe { info => Ofnu(optional, info.multiple, info.containee) }

    // ===========================================================================
    override def toString = formatDefault
      def formatDefault: String =
        //  see t210125111338 (union types)
        if (optional) s"${formatOptional(optional)}\t${infos.map(_.formatDefault).join("|")}"
        else                                           infos.map(_.formatDefault).join("|")

    // ===========================================================================
    private def updateInfos(newInfos: Seq[Info]) = copy(infos = newInfos.distinct)

      def toSingle  : Ofni = updateInfos(newInfos = infos.map(_.toSingle))
      def toMultiple: Ofni = updateInfos(newInfos = infos.map(_.toMultiple))

      def toRequired: Ofni = copy(optional = false)
      def toOptional: Ofni = copy(optional = true)

    // ===========================================================================
    def transformSoleInfo                                  (f: Info => Info)          : Ofni = updateInfos(newInfos = f(info1))
    def transformAllInfos                                  (f: Info => Info)          : Ofni = updateInfos(newInfos = infos.map(f))
    def transformSpecificInfo     (p: Info      => Boolean)(f: Info => Info)          : Ofni = updateInfos(newInfos = infos.mapAffectExactlyOne(p)(f)) // see t210125111338 (union types)
    def transformSpecificContainee(p: Containee => Boolean)(f: Containee => Containee): Ofni = updateInfos(newInfos = infos.mapAffectExactlyOne(_.containee.pipe(p))(_.transformContainee(f)))

    // ---------------------------------------------------------------------------
    def transformNestedClasses                    (f: Cls  => Cls): Ofni = updateInfos(newInfos = infos.mapIf              (_.isNesting)                     (_.transformNestedClass(f)))
    def transformSoleNestedClass                  (f: Cls  => Cls): Ofni = updateInfos(newInfos = infos.mapAffectExactlyOne(_.isNesting)                     (_.transformNestedClass(f)))
    def transformNestedClass(target: Cls)         (f: Cls  => Cls): Ofni = updateInfos(newInfos = infos.mapAffectExactlyOne(_.nestedClassOpt == Some(target))(_.transformNestedClass(f)))
    def transformNestedClass(target: Index)       (f: Cls  => Cls): Ofni = updateInfos(newInfos = infos.mapIndex           (__lookup(target))                (_.transformNestedClass(f)))
    def transformNestedClass(pred: Cls => Boolean)(f: Cls  => Cls): Ofni = updateInfos(newInfos = infos.mapAffectExactlyOne(_.nestedClassOpt.exists(pred))   (_.transformNestedClass(f)))

    def transformNestedClass(target: UnionObjectDisambiguator)(f: Cls  => Cls): Ofni = target match { // TODO: validate first...
        case DisambiguateByClassIndex    (index)   => transformNestedClass(index)(f)
        case DisambiguateByClassPredicate(meta, _) => transformNestedClass(meta) (f) }

    // ---------------------------------------------------------------------------
    def updateSoleInfo                    (newValue: Info): Ofni = transformSoleInfo                   (_ => newValue)
    def updateSpecificInfo(oldValue: Info, newValue: Info): Ofni = transformSpecificInfo(_ == oldValue)(_ => newValue) // see t210125111338 (union types)

    // ---------------------------------------------------------------------------
    def updateContainee(containee: Containee): Ofni = transformSoleInfo(_.updateContainee(containee))
    def updateOptionality(value: Optional): Ofni = copy(optional = value)

    // ===========================================================================
    def potentiallyProcessNesting(value: AnyValue): AnyValue =
      nestedClassOpt
        .map { nestedClass =>
          container1.containerWrap(nestedClass.valueToObj)(value) }
        .getOrElse(value)
  }

  // ===========================================================================
  object Ofni {
    def required(info: Info) = Ofni(_Required, Seq(info))
    def optional(info: Info) = Ofni(_Optional, Seq(info))

    // ---------------------------------------------------------------------------
    def one(containee: Containee): Ofni = Ofni(_Required, Seq(Info(_Single  , containee)))
    def opt(containee: Containee): Ofni = Ofni(_Optional, Seq(Info(_Single, containee)))
    def nes(containee: Containee): Ofni = Ofni(_Required, Seq(Info(_Multiple, containee)))
    def pes(containee: Containee): Ofni = Ofni(_Optional, Seq(Info(_Multiple, containee)))

    // ---------------------------------------------------------------------------
    def one(basic: BasicType.type => BasicType): Ofni = Ofni(_Required, Seq(Info(_Single  , basic(BasicType))))
    def opt(basic: BasicType.type => BasicType): Ofni = Ofni(_Optional, Seq(Info(_Single  , basic(BasicType))))
    def nes(basic: BasicType.type => BasicType): Ofni = Ofni(_Required, Seq(Info(_Multiple, basic(BasicType))))
    def pes(basic: BasicType.type => BasicType): Ofni = Ofni(_Optional, Seq(Info(_Multiple, basic(BasicType))))

    // ---------------------------------------------------------------------------
    def oneBoolean: Ofni = one(_._Boolean)
    def oneInt    : Ofni = one(_._Int)
    def oneDouble : Ofni = one(_._Double)
    def oneString : Ofni = one(_._String)

    // ---------------------------------------------------------------------------
    def optBoolean: Ofni = opt(_._Boolean)
    def optInt    : Ofni = opt(_._Int)
    def optDouble : Ofni = opt(_._Double)
    def optString : Ofni = opt(_._String)

    // ===========================================================================
    def forceFrom[T : WTT]: Ofni = TypeNode.parse[T].forceNonBObjOfni

    // ===========================================================================
    def combine(left: Ofni, right: Ofni): Ofni = { import gallia.vldt.SpecialCardiMode
      require(
        vldt.MetaValidationCompatibility.compatible(left, right, SpecialCardiMode.Normal),
        (left, right))

      val combinedContainer = Container.combine(left.container1, right.container1)

      Ofni(
        optional = combinedContainer.isOptional,
        infos    = Seq(Info(
          combinedContainer.isMultiple,
          Containee.combine(left.info1.containee, right.info1.containee))))
    }
  }

// ===========================================================================
