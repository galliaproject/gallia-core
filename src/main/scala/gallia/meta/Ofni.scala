package gallia
package meta

import aptus._
import domain.SortingPair
import GalliaUtils.Seq__

// ===========================================================================
case class Ofni(optional: Boolean, infos: Seq[Info]) extends OfniLike {
    override protected val _ofni: Ofni = this

    // ---------------------------------------------------------------------------
    infos
      .requireNonEmpty
      .requireDistinct

    // ---------------------------------------------------------------------------
    @deprecated def container: Container = Container.from(optional, info1.multiple)

    // ---------------------------------------------------------------------------
    override def toString = formatDefault
      def formatDefault: String =
        //  see t210125111338 (union types)
        if (optional) s"${formatOptional(optional)}\t${infos.map(_.formatDefault).join("|")}"
        else                                           infos.map(_.formatDefault).join("|")

    // ===========================================================================
    def toRequired: Ofni = copy(optional = false)
    def toOptional: Ofni = copy(optional = true)

    def toSingle  : Ofni = copy(infos    = infos.map(_.toSingle))
    def toMultiple: Ofni = copy(infos    = infos.map(_.toMultiple))

    // ===========================================================================
    def transformSoleInfo                        (f: Info => Info): Ofni = copy(infos = f(info1))
    def transformAllInfos                        (f: Info => Info): Ofni = copy(infos = infos.map(f))
    def transformSpecificInfo(p: Info => Boolean)(f: Info => Info): Ofni = copy(infos = infos.mapAffectExactlyOne(p)(f)) // see t210125111338 (union types)

    // ---------------------------------------------------------------------------
    def transformNestedClasses                   (f: Cls  => Cls): Ofni = copy(infos = infos.mapIf              (_.isNesting)              (_.transformNestedClass(f)))
    def transformSoleNestedClass                 (f: Cls  => Cls): Ofni = copy(infos = infos.mapAffectExactlyOne(_.isNesting)              (_.transformNestedClass(f)))
    def transformNestedClass(name   : ClsName)   (f: Cls  => Cls): Ofni = copy(infos = infos.mapIf              (_.isNestingWithName(name))(_.transformNestedClass(f)))
    def transformNestedClass(nameOpt: ClsNameOpt)(f: Cls  => Cls): Ofni =
      nameOpt
        .map { name => transformNestedClass(name)(f) }
        .getOrElse {   transformSoleNestedClass  (f) }

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
          container.containerWrap(nestedClass.valueToObj)(value) }
        .getOrElse(value)
  }

  // ===========================================================================
  object Ofni {
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

      val combinedContainer = Container.combine(left.container, right.container)

      Ofni(
        optional = combinedContainer.isOptional,
        infos    = Seq(Info(
          combinedContainer.isMultiple,
          Containee.combine(left.info1.containee, right.info1.containee))))
    }
  }

// ===========================================================================